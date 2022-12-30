{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Interface.SystemClock where

import           Include
import           Initialize
import           Interface.Counter
import           Interface.Timer


data SystemClock where
    SystemClock :: (Timer t, Counter c)
                => { timer   :: t
                   , counter :: c
                   }
                -> SystemClock



instance Include SystemClock where
    include (SystemClock {counter}) = include counter

instance Initialize SystemClock where
    initialize (SystemClock {counter}) = initialize counter

instance Counter SystemClock where
    readCounter (SystemClock {counter}) = readCounter counter



instance Include (HandleTimer SystemClock) where
    include (HandleTimer (SystemClock timer _) handle) =
        include (HandleTimer timer handle)

instance Initialize (HandleTimer SystemClock) where
    initialize (HandleTimer (SystemClock timer _) handle) =
        initialize (HandleTimer timer handle)

instance Timer SystemClock
