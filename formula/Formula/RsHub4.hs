module Formula.RsHub4 where

import           Core.Formula
import           Core.Models
import           Data.Color
import           Device.GD32F4xx
import           Feature.DS18B20
import           Feature.Indicator
import qualified Feature.Server     as F
import           Interface.RS485
import           Ivory.Language
import           Transport.UDP.RBUS as U


rsHub4 :: Formula GD32F4xx
rsHub4 = Formula { name       = "rs_hub4"
                 , model      = deviceTypeRsHub4
                 , version    = (4, 0)
                 , shouldInit = true
                 , transport  = U.rbus eth_0
                 , features   = [ indicator npx_pwm_2 50
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
                                           , in_pb_10
                                           , in_pa_15
                                           ]
                                , ds18b20 ow_0 od_pb_3
                                ]
                 }
