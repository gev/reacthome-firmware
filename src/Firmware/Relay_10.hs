{-# LANGUAGE FlexibleInstances #-}

module Firmware.Relay_10 where

import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.USART
import           Feature
import           Feature.RBUS
import           Feature.Relay

relay :: Features
relay =
  [ Feature $ RBUS Slave  1 usart_1
  , Feature $ Relay       1 out_pa_15
  ]
