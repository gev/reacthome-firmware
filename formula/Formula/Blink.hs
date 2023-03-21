module Formula.Blink where

import           Core.Formula
import           Device.GD32F3x0
import qualified Feature.Blink   as F
import           Interface.RS485
import           Ivory.Language
import           Transport.RBUS


blink :: Formula GD32F3x0
blink = Formula { name       = "blink"
                , model      = 0xff
                , version    = (1, 0)
                , shouldInit = false
                , transport  = rbus $ rs485 1 usart_1 out_pa_4
                , features   = [ F.blink 1 out_pa_15 ]
                }
