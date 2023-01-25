{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}

module Scheduler where

import           Control.Monad         (replicateM, zipWithM_)
import           Data.Foldable         (traverse_)
import           Data.List
import           Data.Maybe
import           Feature
import           Include
import           Initialize
import           Interface.SystemClock
import           Interface.Timer
import           Ivory.Language
import           Ivory.Stdlib
import           Util.Data.Class
import           Util.Data.Value


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
    include (Scheduler {clock, steps}) =
        include clock >> traverse_ (incl . runStep) steps

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
