module Formula.Blink where

import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.Timer
import           Device.GD32F3x0.USART         (usart_1)
import           Feature
import           Feature.Blink
import           Feature.USART                 (USART (USART))
import           Formula
import           Support.Device.GD32F3x0.Timer as S


blink :: Formula
blink = Formula { systemClock = timer_2_irq
                                {-
                                  TODO:  use frequency instead timer params
                                -}
                                timerParam { S.prescaler = 8399
                                           , S.period = 9
                                           }
                , features = [ Feature $ Blink 1 out_pa_15
                             , Feature $ USART 1 usart_1
                             ]
                }
