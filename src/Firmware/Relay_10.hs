{-# LANGUAGE FlexibleInstances #-}

module Firmware.Relay_10 where

import           Device.GPIO
import           Device.MCU.GD32F3x0.GPIO
import           Feature
import           Ivory.Language.Module

relay :: Features
relay =
  [ Feature $ RBUS Slave  1 usart_1
  , Feature $ Relay       1 out_15
  ]

instance Prepare (RBUS (GPIO MCU_GPIO)) where
  prepare = undefined

instance Prepare (Relay (GPIO MCU_GPIO)) where
  prepare = undefined
