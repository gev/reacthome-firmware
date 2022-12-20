{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Interface.SystemClock where

import           Include
import           Initialize
import qualified Interface.Counter as I
import qualified Interface.Timer   as I


data SystemClock where
  SystemClock :: (I.Timer t, I.Counter c)
              => { timer    :: t
                 , counter  :: c
                 }
              -> SystemClock



instance Include SystemClock where
  include (SystemClock {counter}) = include counter

instance Initialize SystemClock where
  initialize (SystemClock {counter}) = initialize counter

instance I.Counter SystemClock where
  readCounter (SystemClock {counter}) = I.readCounter counter



instance Include (I.HandleTimer SystemClock) where
  include (I.HandleTimer (SystemClock {timer}) handle) =
    include (I.HandleTimer timer handle)

instance Initialize (I.HandleTimer SystemClock) where
  initialize (I.HandleTimer (SystemClock {timer}) handle) =
    initialize (I.HandleTimer timer handle)

instance I.Timer SystemClock
