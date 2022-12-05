{-# LANGUAGE GADTs #-}

module Feature.RBUS where

import           Feature
import           Interface       as I
import           Interface.RS485 as I

data RBUS = RBUS RBUS' Int I.RS485

data RBUS'
  = Master
  | Slave

instance Interface (RBUS ) where
  initialize = undefined
  dependencies = undefined

instance Task (RBUS ) where
  task = undefined
