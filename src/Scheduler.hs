{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}

module Scheduler where

import           Control.Monad   (replicateM, zipWithM_)
import           Data.List
import           Data.Maybe
import           Feature
import           Interface       as I
import qualified Interface.IRQ   as Q
import           Interface.Timer as I
import           Ivory.Language
import           Ivory.Stdlib


data Scheduler = forall t. Q.IRQ t => Scheduler
  { timer :: t
  , steps :: [Step]
  }

instance I.Interface Scheduler  where

  dependencies (Scheduler {timer, steps}) =
    [defMemArea schedulerTimer, Q.handleIRQ timer handleIRQ]
    <> I.dependencies timer
    <> (incl . step <$> steps)


  initialize (Scheduler {timer, steps}) = I.initialize timer

schedulerTimer :: MemArea ('Stored Uint32)
schedulerTimer = area "scheduler_timer" (Just (ival 0))

handleIRQ :: Ivory (ProcEffects s ()) ()
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
