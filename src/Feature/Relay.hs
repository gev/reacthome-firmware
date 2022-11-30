module Feature.Relay where

import           Feature
import           Interface.GPIO

data Relay a = Relay Int a

instance OUT o => Prepare (Relay o) where
  prepare = undefined

