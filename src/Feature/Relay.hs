{-# LANGUAGE GADTs #-}

module Feature.Relay where

import           Feature
import           Interface
import           Interface.GPIO

data Relay a = (OUT a) => Relay Int a

instance Interface (Relay o) where
  initialize = undefined
  dependencies = undefined

instance Task (Relay o) where
  step = undefined
