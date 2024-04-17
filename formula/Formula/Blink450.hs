module Formula.Blink450 where

import           Core.Formula
import           Device.GD32F4xx
import           Implementation.Blink (blink)
import           Ivory.Language
import           Transport.UART.RBUS


blink450 :: Formula GD32F4xx
blink450 = Formula { name           = "blink450"
                   , model          = 0xff
                   , version        = (1, 0)
                   , shouldInit     = false
                   , implementation = blink out_pd_12
                   }
