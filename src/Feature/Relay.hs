{-# LANGUAGE GADTs #-}

module Feature.Relay where

import           Feature
import           Include
import           Initialize
import           Interface.GPIO

data Relay = forall a. (OUT a) => Relay Int a

instance Include Relay where
  include = undefined

instance Initialize Relay where
  initialize = undefined

instance Task Relay where
  tasks = undefined
