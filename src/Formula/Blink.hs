module Formula.Blink where

import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.SystemClock
import           Device.GD32F3x0.USART       (usart_1)
import           Feature.Blink               as F
import           Feature.RBUS
import           Formula
import           Interface.RS485


blink :: Formula
blink = Formula { clock = systemClock
                , features = [ F.blink 1 out_pa_15
                             , rbus    1 $ RS485 usart_1 out_pa_4
                             ]
                }
