module Formula.Blink where

import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.SystemClock
import           Device.GD32F3x0.USART       (usart_1)
import           Feature.Blink               as B
import           Feature.RS485
import           Formula
import           Interface.RS485             as I


blink :: Formula
blink = Formula { clock    = systemClock
                , features = [ B.blink 1 out_pa_15
                             , rs485   1 $ I.RS485 usart_1 out_pa_4
                             ]
                }
