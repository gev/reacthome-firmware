module Formula.Smart.Bottom.Bottom1 where

import           Core.Formula
import           Device.GD32F3x0
import           Feature.DInputs
import           Feature.DS18B20      (ds18b20)
import           Feature.Smart.Top
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS

smartBottom1 :: Formula GD32F3x0
smartBottom1 =  Formula { name       = "smartBottom1"
                        , model      = 0x20
                        , version    = (4, 3)
                        , shouldInit = false
                        , transport  = rbus $ rs485 1 uart_1 out_pa_4
                        , features   = [ top uart_0 in_pb_4
                                       , dinputs [  in_pb_1
                                                 ,  in_pb_0
                                                 ,  in_pa_6
                                                 ,  in_pa_5
                                                 ]
                                       , ds18b20 ow_0 od_pa_15
                                       ]
                        }
