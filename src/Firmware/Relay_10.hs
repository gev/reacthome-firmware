module Firmware.Relay_10 where

import           Device.Feature
import           Device.GPIO
import           Device.MCU.GD32F3x0.GPIO

relay :: [Feature (GPIO MCU_GPIO)]
relay =
  [ RBUS Slave  1 usart_1
  , Relay       1 out_15
  ]
