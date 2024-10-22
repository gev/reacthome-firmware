{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}

module Interface.SystemClock where

import           Control.Monad.State
import           Core.Context
import           Core.Handler        (Handler (addHandler))
import           Data.Value
import           Interface.Counter   (Counter (readCounter))
import           Interface.Timer     (HandleTimer (HandleTimer), Timer)
import           Ivory.Language



data SystemClock where
    SystemClock :: Timer t
                => { timer   :: t
                   , time    :: Value Uint32
                   }
                -> SystemClock


systemClock :: (MonadState Context m, Timer t)
            => m t -> m SystemClock
systemClock timer' = do
    time    <- value "system_time" 0
    timer   <- timer'
    let handle' :: Ivory eff ()
        handle' = do
            t <- deref time
            store time $ t + 1
    addHandler $ HandleTimer timer handle'
    pure SystemClock { timer, time }



getSystemTime :: SystemClock -> Ivory eff Uint32
getSystemTime = deref . time
