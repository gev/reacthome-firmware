module Formula.Blink where

import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.SystemClock
import           Device.GD32F3x0.USART       (usart_1)
import           Feature
import           Feature.Blink
import           Feature.USART               (USART (USART))
import           Formula


blink :: Formula
blink = Formula { clock    = systemClock
                , features = [ Feature $ Blink 1 out_pa_15
                             , Feature $ Blink 2 out_pa_4
                             , Feature $ USART 1 usart_1
                             ]
                }
