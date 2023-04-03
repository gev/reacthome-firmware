module Formula.Server where

import           Core.Formula
import           Device.GD32F4xx
import           Feature.RS485.RBUS  as F
import           Interface.RS485
import           Ivory.Language
import           Transport.UART.RBUS as U


server :: Formula GD32F4xx
server = Formula { name       = "server"
                 , model      = 0xff
                 , version    = (1, 0)
                 , shouldInit = false
                 , transport  = U.rbus uart_7
                 , features   = [ F.rbus [ rs485 1 uart_0 out_pc_6
                                         , rs485 2 uart_1 out_pd_7
                                         , rs485 3 uart_2 out_pa_15
                                         , rs485 4 uart_5 out_pd_15
                                         ]
                                ]
                 }
