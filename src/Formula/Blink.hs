module Formula.Blink where

import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.SystemClock
import           Device.GD32F3x0.USART       (USART, usart_1)
import           Feature.Blink               as B
import           Feature.USART
import           Formula


blink :: Formula
blink = Formula { clock    = systemClock
                , features = [ B.blink 1 out_pa_15
                             , B.blink 2 out_pa_4
                             , usart   1 usart_1
                             ]
                }
