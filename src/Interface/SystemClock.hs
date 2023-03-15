{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RecordWildCards   #-}

module Interface.SystemClock where

import           Core.Context
import           Data.Value
import           Interface.Counter (Counter (readCounter))
import           Interface.Timer   (HandleTimer (HandleTimer), Timer)
import           Ivory.Language



data SystemClock where
    SystemClock :: (Timer t, Counter c)
                => { timer   :: t
                   , counter :: c
                   , time    :: Value Uint32
                   }
                -> SystemClock

systemClock timer counter = SystemClock
    { timer   = timer
    , counter = counter
    , time    = value "system_time" 0
    }



instance Include SystemClock where
    include (SystemClock {..}) = do
        include time
        include counter
        include (HandleTimer timer $ handle time)



handle :: Value Uint32 -> Ivory eff ()
handle time = do
    t <- deref $ addrOf time
    store (addrOf time) $ t + 1



instance Counter SystemClock where
    readCounter (SystemClock {..}) = readCounter counter



getSystemTime :: SystemClock -> Ivory eff Uint32
getSystemTime = deref . addrOf . time
