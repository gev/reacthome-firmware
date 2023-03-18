{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}

module Interface.SystemClock where

import           Control.Monad.Writer
import           Core.Context
import           Core.Handler         (Handler (addHandler))
import           Data.Value
import           Interface.Counter    (Counter (readCounter))
import           Interface.Timer      (HandleTimer (HandleTimer), Timer)
import           Ivory.Language



data SystemClock where
    SystemClock :: (Timer t, Counter c)
                => { timer   :: t
                   , counter :: c
                   , time    :: Value Uint32
                   }
                -> SystemClock


systemClock :: (MonadWriter Context m, Timer t, Counter c)
            => m t -> m c -> m SystemClock
systemClock timer' counter' = do
    time    <- value "system_time" 0
    timer   <- timer'
    counter <- counter'
    let handle' :: Ivory eff ()
        handle' = do
            t <- deref $ addrOf time
            store (addrOf time) $ t + 1
    addHandler $ HandleTimer timer handle'
    pure SystemClock { timer, counter, time }



getSystemTime :: SystemClock -> Ivory eff Uint32
getSystemTime = deref . addrOf . time


instance Counter SystemClock where
    readCounter (SystemClock {..}) = readCounter counter
