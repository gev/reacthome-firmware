module Core.Scheduler where

import Control.Monad (replicateM, zipWithM_)
import Core.Task
import Data.List
import Data.Maybe
import Interface.SystemClock
import Ivory.Language
import Ivory.Stdlib

mkLoop :: SystemClock -> [Task] -> Def ('[] :-> ())
mkLoop systemClock tasks = proc "loop" $ body do
    let (scheduled, immediately) = partition (isJust . period) tasks
    clocks <- replicateM (length scheduled) (local (ival 0))
    forever do
        t <- getSystemTime systemClock
        zipWithM_ (run t) clocks scheduled
        mapM_ runTask immediately
  where
    run t1 clock task = do
        t0 <- deref clock
        let Period interval phase = fromJust $ period task
        when (t1 - t0 >=? interval + phase) do
            runTask task
            store clock $ t1 - phase
