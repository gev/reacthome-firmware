module Formula.Relay_10 where

import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.Timer
import           Device.GD32F3x0.USART
import           Feature
import           Feature.RBUS
import           Feature.Relay
import           Formula
import           Support.Device.GD32F3x0.Timer


relay :: Formula
relay = Formula { systemClock = timer_2_irq
                                timerParam { prescaler = 8399
                                           , period = 9
                                           }
                , features = [ Feature $ RBUS Slave  1 usart_1
                             , Feature $ Relay       1 out_pa_15
                             ]
                }
