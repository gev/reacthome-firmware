module Formula.Server where

import           Core.Formula
import           Data.Color
import           Device.GD32F4xx
import           Feature.Indicator
import qualified Feature.Server      as F
import           Feature.DS18B20 (ds18b20)
import           Interface.RS485
import           Ivory.Language
import           Transport.UART.RBUS as U


server :: Formula GD32F4xx
server = Formula { name       = "server"
                 , model      = 0xc0
                 , version    = (3, 0)
                 , shouldInit = true
                 , transport  = U.rbus uart_1
                 , features   = [ indicator npx_pwm_2 120
                                , F.server [ rs485 1 uart_5 out_pb_14
                                           , rs485 2 uart_3 out_pc_12
                                           , rs485 3 uart_2 out_pb_15
                                           , rs485 4 uart_0 out_pd_1
                                           ]
                                           [ pwm_0
                                           , pwm_1
                                           , pwm_2
                                           ]
                                           [ in_pa_8
                                           , in_pb_4
                                           , in_pe_3
                                           , in_pe_2
                                           ]
                                , ds18b20 ow_0 od_pb_3
                                ]
                 }
