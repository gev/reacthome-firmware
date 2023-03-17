{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Core.Scheduler where

import           Control.Monad         (replicateM, zipWithM_)
import           Core.Feature
import           Core.Task
import           Data.List
import           Data.Maybe
import           Data.Value
import           Interface.SystemClock
import           Interface.Timer
import           Ivory.Language
import           Ivory.Stdlib


data Scheduler = Scheduler
    { clock :: SystemClock
    , steps :: [Task]
    }

scheduler :: SystemClock -> [Task] -> Scheduler
scheduler = Scheduler



schedule :: Scheduler -> Def ('[] :-> ())
schedule (Scheduler {..}) = proc "loop" $ body $ do
    let (scheduled, immediately) = partition (isJust . period) steps
    clocks <- replicateM (length scheduled) (local (ival 0))
    forever $ do
        t <- getSystemTime clock
        zipWithM_ (run t) clocks scheduled
        mapM_ (call_ . runTask) immediately



run :: Uint32 -> Ref ('Stack s) ('Stored Uint32) -> Task -> Ivory eff ()
run t c s = do
    v <- deref c
    when (t - v >=? fromJust (period s)) $ do
        call_ $ runTask s
        store c t
