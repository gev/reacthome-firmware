{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}

module Scheduler where

import           Control.Monad         (replicateM, zipWithM_)
import           Data.List
import           Data.Maybe
import           Feature
import           Interface
import           Interface.SystemClock
import           Interface.Timer      
import           Ivory.Language
import           Ivory.Stdlib


data Scheduler = Scheduler
  { clock :: SystemClock
  , steps :: [Step]
  }

instance Interface Scheduler  where

  dependencies (Scheduler clock steps) = defMemArea schedulerTimer
                                       : dependencies clock
                                      <> dependencies (HandleTimer clock handleIRQ)
                                      <> (incl . step <$> steps)


  initialize (Scheduler {clock}) =
    initialize clock <> initialize (HandleTimer clock handleIRQ)

schedulerTimer :: MemArea ('Stored Uint32)
schedulerTimer = area "scheduler_timer" (Just (ival 0))

handleIRQ :: Ivory eff ()
handleIRQ = do
  let c = addrOf schedulerTimer
  v <- deref c
  store c $ v + 1

schedule :: Scheduler -> Def ('[] :-> ())
schedule (Scheduler {steps}) = proc "loop" $ body $ do
  let (scheduled, immediately) = partition (isJust . period) steps
  clocks <- replicateM (length scheduled) (local (ival 0))
  forever $ do
    t <- deref $ addrOf schedulerTimer
    zipWithM_ (run t) clocks scheduled
    mapM_ (call_ . step) immediately


run :: Uint32 -> Ref ('Stack s) ('Stored Uint32) -> Step -> Ivory eff ()
run t c s = do
  v <- deref c
  when (t - v >=? fromJust (period s)) $ do
    call_ $ step s
    store c t
