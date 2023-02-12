module Formula.Blink where

import           Data.Function   ((&))
import           Device.GD32F3x0
import           Feature.Blink   as F
import           Feature.RBUS
import           Formula
import           Interface.RS485

blink :: Formula
blink = Formula { mcu       = gd32ffx0
                , features  = [ F.blink 1 out_pa_15
                              ]
                }
