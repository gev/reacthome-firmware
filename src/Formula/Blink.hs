module Formula.Blink where

import           Device.GD32F3x0.GPIO
import           Feature
import           Feature.Blink


blink :: Features
blink =
  [ Feature $ Blink 1 out_pa_15
  ]
