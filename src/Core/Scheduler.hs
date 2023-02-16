{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}

module Core.Scheduler where

import           Control.Monad         (replicateM, zipWithM_)
import           Core.Feature
import           Core.Include
import           Core.Initialize
import           Data.Class
import           Data.Foldable         (traverse_)
import           Data.List
import           Data.Maybe
import           Data.Value
import           Interface.SystemClock
import           Interface.Timer
import           Ivory.Language
import           Ivory.Stdlib


data Scheduler = Scheduler
    { clock :: SystemClock
    , steps :: [Step]
    }

scheduler :: SystemClock -> [Step] -> Scheduler
scheduler clock steps =  Scheduler
    { clock = clock
    , steps = steps
    }



instance Include Scheduler where
    include (Scheduler {clock}) =
        include clock

instance Initialize Scheduler where
    initialize (Scheduler {clock}) = initialize clock



schedule :: Scheduler -> Def ('[] :-> ())
schedule (Scheduler {clock, steps}) = proc "loop" $ body $ do
    let (scheduled, immediately) = partition (isJust . period) steps
    clocks <- replicateM (length scheduled) (local (ival 0))
    forever $ do
        t <- getSystemTime clock
        zipWithM_ (run t) clocks scheduled
        mapM_ (call_ . runStep) immediately



run :: Uint32 -> Ref ('Stack s) ('Stored Uint32) -> Step -> Ivory eff ()
run t c s = do
    v <- deref c
    when (t - v >=? fromJust (period s)) $ do
        call_ $ runStep s
        store c t