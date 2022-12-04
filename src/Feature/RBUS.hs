{-# LANGUAGE GADTs #-}

module Feature.RBUS where

import           Feature
import           Interface
import           Interface.USART

data RBUS a = (USART a) => RBUS RBUS' Int a

data RBUS'
  = Master
  | Slave

instance Interface (RBUS u) where
  initialize = undefined
  dependencies = undefined

instance Task (RBUS u) where
  task = undefined
