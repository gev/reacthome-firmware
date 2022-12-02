{-# LANGUAGE GADTs #-}

module Feature.Relay where

import           Feature
import           Interface.GPIO
import           Prepare

data Relay a = (OUT a) => Relay Int a

instance Prepare (Relay o) where
  prepare = undefined
