{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Interface.SystemClock where

import           Data.Class
import           Data.Value
import           Include
import           Initialize
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
    include (SystemClock {timer, counter, time}) =
        include counter >> include time >>
        include (HandleTimer timer $ handle time)

instance Initialize SystemClock where
    initialize (SystemClock {timer, counter, time}) =
        initialize counter <>
        initialize (HandleTimer timer $ handle time)

handle :: Value Uint32 -> Ivory eff ()
handle time = do
    t <- getValue time
    setValue time $ t + 1



instance Counter SystemClock where
    readCounter (SystemClock {counter}) = readCounter counter



getSystemTime :: SystemClock -> Ivory eff Uint32
getSystemTime = getValue . time
