module Formula.UdpEcho450 where

import           Core.Formula
import           Device.GD32F4xx
import           Implementation.UdpEcho (udpEcho)
import           Ivory.Language
import           Transport.UART.RBUS


udpEcho450 :: Formula GD32F4xx
udpEcho450 = Formula { name           = "udpEcho450"
                     , model          = 0xff
                     , version        = (1, 0)
                     , shouldInit     = false
                     , transport      = rbus uart_5
                     , implementation = udpEcho eth_0
                     }
