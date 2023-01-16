module Formula.Relay_10 where

import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.SystemClock
import           Device.GD32F3x0.USART
import           Feature.RBUS
import           Feature.Relay               as F
import           Formula
import           Interface.RS485


relay :: Formula
relay = Formula { clock    = systemClock
                , features = [ rbus    1 $ RS485 1 usart_1 out_pa_4
                             , F.relay 1 out_pa_15
                             ]
                }
