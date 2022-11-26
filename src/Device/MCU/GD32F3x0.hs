module Device.MCU.GD32F3x0 where

import           Device.GPIO

mcu :: GPIO Int
mcu = gpio
  { pio   = (PA <$> [0..15]) <> (PB <$> [0..8])
  , usart = [USART { rx = 1, tx = 1 }]
  }

