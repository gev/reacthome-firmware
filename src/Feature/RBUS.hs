{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Feature.RBUS where

import           Feature
import           Include
import           Initialize
import           Interface.RS485 as I
import           Interface.USART as I

data RBUS = RBUS RBUS' Int I.RS485

data RBUS'
  = Master
  | Slave

instance Include RBUS where
  include = undefined

instance Initialize RBUS where
  initialize = undefined

instance Task RBUS where
  tasks = undefined
