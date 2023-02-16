module Formula.Blink where

import           Core.Formula
import           Device.GD32F3x0
import           Feature.Blink   as F


blink :: Formula
blink = Formula { model     = 0xff
                , version   = (1, 0)
                , mcu       = gd32f3x0
                , features  = [ F.blink 1 out_pa_15
                              ]
                }
