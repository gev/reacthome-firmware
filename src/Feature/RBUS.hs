{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Feature.RBUS where

import           Feature
import           Interface       as I
import           Interface.RS485 as I
import           Interface.USART as I

data RBUS = RBUS RBUS' Int (I.OnReceive -> I.RS485)

data RBUS'
  = Master
  | Slave

instance Interface RBUS where
  initialize = undefined
  dependencies = undefined

instance Task RBUS where
  tasks = undefined
