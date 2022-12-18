{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Interface.SystemClock where

import           Interface
import qualified Interface.Counter as I
import qualified Interface.Timer   as I


data SystemClock where
  SystemClock :: (I.Timer t, I.Counter c)
              => { timer    :: t
                 , counter  :: c
                 }
              -> SystemClock



instance Interface SystemClock where

  dependencies (SystemClock {counter}) = dependencies counter

  initialize (SystemClock {counter}) = initialize counter


instance I.Counter SystemClock where
  readCounter (SystemClock {counter}) = I.readCounter counter




instance Interface (I.HandleTimer SystemClock) where

  dependencies (I.HandleTimer (SystemClock {timer}) handle) =
    dependencies (I.HandleTimer timer handle)

  initialize (I.HandleTimer (SystemClock {timer}) handle) =
    initialize (I.HandleTimer timer handle)


instance I.Timer SystemClock
