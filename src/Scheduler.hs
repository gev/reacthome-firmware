{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}

module Scheduler where

import           Control.Monad   (replicateM, replicateM_, zipWithM_)
import           Data.Maybe      (fromJust, isJust, isNothing)
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
    [defMemArea clock, Q.handleIRQ timer handleIRQ]
    <> I.dependencies timer
    <> (incl . step <$> steps)


  initialize (Scheduler {timer, steps}) = I.initialize timer

clock :: MemArea ('Stored Uint32)
clock = area "scheduler_clock" (Just (ival 0))

handleIRQ :: Ivory (ProcEffects s ()) ()
handleIRQ = do
  let c = addrOf clock
  v <- deref c
  store c $ v + 1

schedule :: Scheduler -> Def ('[] :-> ())
schedule (Scheduler {steps}) = proc "loop" $ body $ do
  let immediately = filter (isNothing . period) steps
  let scheduled = filter (isJust . period) steps
  clocks <- replicateM (length scheduled) (local (ival 0))
  forever $ do
    t <- deref $ addrOf clock
    zipWithM_ (run t) clocks scheduled
    mapM_ (call_ . step) immediately


run :: Uint32 -> Ref ('Stack s) ('Stored Uint32) -> Step -> Ivory eff ()
run t c s = do
  v <- deref c
  when (t - v >=? fromJust (period s)) $ do
    call_ $ step s
    store c t
