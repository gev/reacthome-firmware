module Firmware.Blink (blink) where

import           Device.MCU.GD32F3x0.OUT
import           Feature
import           Feature.Blink


blink :: Features
blink =
  [ Feature $ Blink 1 out_pa_15
  ]
