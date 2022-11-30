module Feature.RBUS where

import           Feature
import           Interface.GPIO

data RBUS a = RBUS RBUS' Int a

data RBUS'
  = Master
  | Slave

instance USART u => Prepare (RBUS u) where
  prepare = undefined
