module Formula.Server where

import           Core.Formula
import           Core.Models
import           Data.Color
import           Device.GD32F4xx
import           Feature.Dimmers.DC
import           Feature.DInputs
import           Feature.DS18B20
import           Feature.Indicator
import           Feature.RS485.RBUS  as F
import           Implementation.Hub  (hub)
import           Interface.RS485
import           Ivory.Language
import           Transport.UART.RBUS as U


server :: Formula GD32F4xx
server = Formula { name           = "server"
                 , model          = deviceTypeServer
                 , version        = (5, 0)
                 , shouldInit     = true
                 , implementation = hub (U.rbus uart_1)
                                        (F.rbus    [ rs485 uart_5 out_pb_14
                                                   , rs485 uart_3 out_pc_12
                                                   , rs485 uart_2 out_pb_15
                                                   , rs485 uart_0 out_pd_1
                                                   ]
                                        )
                                        (dimmersDC [ pwm_0
                                                   , pwm_1
                                                   , pwm_2
                                                   ]
                                        )
                                        (dinputs   [ in_pa_8
                                                   , in_pb_4
                                                   , in_pe_3
                                                   , in_pe_2
                                                   ]
                                        )
                                        (ds18b20 ow_0 od_pb_3)
                                        (indicator npx_pwm_2 120)
                 }
