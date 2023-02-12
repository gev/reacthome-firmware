module Formula.Blink where

import           Data.Function   ((&))
import           Device.GD32F3x0 (gd32ffx0, out_pa_15, out_pa_4, usart_1)
import qualified Device.GD32F3x0 as G
import           Feature.Blink   as F
import           Feature.RBUS
import           Formula
import           Interface.RS485

blink :: Formula
blink = Formula { mac       = gd32ffx0 & G.mac
                , clock     = gd32ffx0 & G.clock
                , features  = [ F.blink 1 $ gd32ffx0 & out_pa_15
                              ]
                }
