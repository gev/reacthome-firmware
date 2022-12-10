module Formula.Relay_10 where

import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.SystemTimer
import           Device.GD32F3x0.USART
import           Feature                       (Feature (..))
import           Feature.RBUS
import           Feature.Relay
import           Formula
import           Interface.RS485
import           Support.Device.GD32F3x0.Timer


relay :: Formula
relay = Formula { systemClock = systemTimer
                , features = [ Feature $ RBUS Slave  1 (RS485 usart_1 out_pa_4)
                             , Feature $ Relay       1 out_pa_15
                             ]
                }
