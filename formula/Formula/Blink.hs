module Formula.Blink where

import           Core.Formula
import           Device.GD32F3x0
import           Feature.Blink   as F
import           Interface.RS485
import           Transport.RBUS


blink :: Formula
blink = Formula { model     = 0xff
                , version   = (1, 0)
                , mcu       = gd32f3x0
                , transport = rbus $ rs485 1 usart_1 out_pa_4
                , features  = [ F.blink 1 out_pa_15 ]
                }
