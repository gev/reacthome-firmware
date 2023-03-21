module Formula.Blink330 where

import           Core.Formula
import           Device.GD32F3x0
import           Feature.Blink   (blink)
import           Interface.RS485
import           Ivory.Language
import           Transport.RBUS


blink330 :: Formula GD32F3x0
blink330 = Formula { name       = "blink330"
                   , model      = 0xff
                   , version    = (1, 0)
                   , shouldInit = false
                   , transport  = rbus $ rs485 1 usart_1 out_pa_4
                   , features   = [ blink 1 out_pa_15 ]
                   }
