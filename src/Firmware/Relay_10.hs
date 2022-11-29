{-# LANGUAGE FlexibleInstances #-}

module Firmware.Relay_10 where

import           Device.GPIO              as D
import           Device.MCU.GD32F3x0.GPIO
import           Feature
import           Ivory.Language.Module

relay :: Features
relay =
  [ Feature $ RBUS Slave  1 usart_1
  , Feature $ Relay       1 out_15
  ]

instance D.USART u => Prepare (RBUS u) where
  prepare = undefined

instance D.OUT o => Prepare (Relay o) where
  prepare = undefined
