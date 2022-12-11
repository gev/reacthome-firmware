module Formula.Relay_10 where

import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.SystemClock
import           Device.GD32F3x0.USART
import           Feature                     (Feature (..))
import           Feature.RBUS
import           Feature.Relay
import           Formula
import           Interface.RS485


relay :: Formula
relay = Formula { clock    = systemClock
                , features = [ Feature $ RBUS Slave  1 (RS485 usart_1 out_pa_4)
                             , Feature $ Relay       1 out_pa_15
                             ]
                }
