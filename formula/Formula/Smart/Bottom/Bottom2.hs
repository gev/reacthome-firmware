module Formula.Smart.Bottom.Bottom2 where

import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F3x0
import           Feature.ALED                (aled)
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
                        , implementation = bottom2 (rbus $ rs485 uart_1 out_pa_4)
                                                   (top uart_0 in_pb_4)
                                                   (dinputs $  in_pa_5
                                                            :> in_pa_6
                                                            :> in_pb_0
                                                            :> in_pb_1
                                                            :> Nil
                                                   )
                                                   (ds18b20 ow_0 od_pa_15)
                                                   (scd40 i2c_0)
                                                   (aled npx_pwm_3)
                        }
