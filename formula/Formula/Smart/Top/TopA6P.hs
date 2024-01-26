module Formula.Smart.Top.TopA6P where

import           Core.Formula
import           Core.Models
import           Device.GD32F3x0
import           Feature.DInputs
import           Feature.Sht21
import           Implementation.Smart.Top
import           Ivory.Language
import           Transport.UART.RBUS

smartTopA6P :: Formula GD32F3x0
smartTopA6P =  Formula { name           = "smart_top_a6p"
                       , model          = deviceTypeSmartTopA6P
                       , version        = (1, 0)
                       , shouldInit     = false
                       , transport      = rbus uart_0
                       , implementation = top (dinputs [ in_pa_4
                                                       , in_pb_2
                                                       , in_pa_5
                                                       , in_pb_1
                                                       , in_pa_6
                                                       , in_pb_0
                                                       ]
                                              )
                                              (sht21 i2c_0)
                                            -- npx_pwm_0 [led1, led6, led2, led5, led3, led4]
                       }