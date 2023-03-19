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
mkLoop clock tasks = proc "loop" $ body $ do
    let (scheduled, immediately) = partition (isJust . period) tasks
    clocks <- replicateM (length scheduled) (local (ival 0))
    forever $ do
        t <- getSystemTime clock
        zipWithM_ (run t) clocks scheduled
        mapM_ (call_ . runTask) immediately
    where
        run t c s = do
            v <- deref c
            when (t - v >=? fromJust (period s)) $ do
                call_ $ runTask s
                store c t
