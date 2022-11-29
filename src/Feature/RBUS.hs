module Feature.RBUS where

import           Device.GPIO
import           Feature

data RBUS a = RBUS RBUS' Int a

data RBUS'
  = Master
  | Slave

instance USART u => Prepare (RBUS u) where
  prepare = undefined
