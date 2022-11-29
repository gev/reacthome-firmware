module Firmware.Blink (blink) where

import           Device.GPIO
import           Device.MCU.GD32F3x0.GPIO
import           Feature
import           Feature.Blink


blink :: Features
blink =
  [ Feature $ Blink 1 out_15
  ]
