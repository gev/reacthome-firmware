{-# LANGUAGE GADTs #-}

module Feature.RBUS where

import           Feature
import           Interface.USART
import           Prepare

data RBUS a = (USART a) => RBUS RBUS' Int a

data RBUS'
  = Master
  | Slave

instance Prepare (RBUS u) where
  prepare = undefined
