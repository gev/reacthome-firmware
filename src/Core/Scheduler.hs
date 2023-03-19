{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Core.Scheduler where

import           Control.Monad         (replicateM, zipWithM_)
import           Core.Task
import           Data.List
import           Data.Maybe
import           Interface.SystemClock
import           Interface.Timer
import           Ivory.Language
import           Ivory.Stdlib


mkLoop :: SystemClock -> [Task] -> Def ('[] :-> ())
mkLoop systemClock tasks = proc "loop" $ body $ do
    let (scheduled, immediately) = partition (isJust . period) tasks
    clocks <- replicateM (length scheduled) (local (ival 0))
    forever $ do
        t <- getSystemTime systemClock
        zipWithM_ (run t) clocks scheduled
        mapM_ (call_ . runTask) immediately
    where
        run t1 clock task = do
            t0 <- deref clock
            when (t1 - t0 >=? fromJust (period task)) $ do
                call_ $ runTask task
                store clock t1
