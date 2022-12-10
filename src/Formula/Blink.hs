module Formula.Blink where

import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.SystemTimer
import           Device.GD32F3x0.USART         (usart_1)
import           Feature
import           Feature.Blink
import           Feature.USART                 (USART (USART))
import           Formula
import           Support.Device.GD32F3x0.Timer


{-
    TODO:  use frequency instead timer params
-}
blink :: Formula
blink = Formula { systemClock = systemTimer
                , features = [ Feature $ Blink 1 out_pa_15
                             , Feature $ Blink 2 out_pa_4
                             , Feature $ USART 1 usart_1
                             ]
                }
