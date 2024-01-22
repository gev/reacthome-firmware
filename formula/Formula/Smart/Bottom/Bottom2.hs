module Formula.Smart.Bottom.Bottom2 where

import           Core.Formula
import           Core.Models
import           Device.GD32F3x0
import           Feature.DInputs
import           Feature.DS18B20
import           Feature.Scd40
import           Feature.Sht21
import           Feature.Smart.Top
import           Implementation.Smart.Bottom (bottom2)
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS

smartBottom2 :: Formula GD32F3x0
smartBottom2 =  Formula { name           = "smart_bottom_2"
                        , model          = deviceTypeSmartBottom2
                        , version        = (1, 0)
                        , shouldInit     = false
                        , transport      = rbus $ rs485 uart_1 out_pa_4
                        , implementation = bottom2 (top uart_0 in_pb_4)
                                                   (dinputs [ in_pb_1
                                                            , in_pb_0
                                                            , in_pa_6
                                                            , in_pa_5
                                                            ]
                                                   )
                                                   (ds18b20 ow_0 od_pa_15)
                                                   (scd40 i2c_0)
                        }
