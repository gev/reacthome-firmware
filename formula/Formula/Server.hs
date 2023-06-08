module Formula.Server where

import           Core.Formula
import           Data.Color
import           Device.GD32F4xx
import           Feature.Indicator
import           Feature.RS485.RBUS  as F
import           Interface.RS485
import           Ivory.Language
import           Transport.UART.RBUS as U


server :: Formula GD32F4xx
server = Formula { name       = "server"
                 , model      = 0xff
                 , version    = (1, 0)
                 , shouldInit = true
                 , transport  = U.rbus uart_1
                 , features   = [ indicator npx_pwm_2 120
                                , F.rbus [ rs485 1 uart_0 out_pd_1
                                         , rs485 2 uart_2 out_pb_15
                                         , rs485 3 uart_3 out_pc_12
                                         , rs485 4 uart_5 out_pb_14
                                         ]
                                ]
                 }
