module Formula.Smart.SmartTop.SmartTopA6P where

import           Core.Formula
import           Device.GD32F3x0
import           Feature.DInputs
import           Feature.DS18B20      (ds18b20)
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS

smartTopA6P ::  Formula GD32F3x0
smartTopA6P =   Formula { name       = "smartTopA6P"
                        , model      = 0x20
                        , version    = (4, 3)
                        , shouldInit = false
                        , transport  = rbus $ rs485 1 uart_1 out_pa_4
                        , features   = [ dinputs [  in_pa_12
                                                 ,  in_pa_11
                                                 ,  in_pa_10
                                                 ,  in_pa_9
                                                 ]
                                    --    , ds18b20 ow_0 od_pa_8
                                       ]
                        }
