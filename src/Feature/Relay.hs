{-# LANGUAGE GADTs #-}

module Feature.Relay where

import           Feature
import           Interface
import           Interface.GPIO

data Relay = forall a. (OUT a) => Relay Int a

instance Interface Relay where
  initialize = undefined
  dependencies = undefined

instance Task Relay where
  task = undefined
