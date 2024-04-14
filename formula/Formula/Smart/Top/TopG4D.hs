module Formula.Smart.Top.TopG4D where

import           Core.Formula
import           Core.Models
import           Device.GD32F3x0
import           Feature.DInputs            (dinputs)
import           Feature.Sht21              (sht21)
import           Implementation.Smart.TopGD (topGD)
import           Ivory.Language
import           Transport.UART.RBUS        (rbus)

smartTopG4D :: Formula GD32F3x0
smartTopG4D =  Formula { name           = "smart_top_g4d"
                       , model          = deviceTypeSmartTopG4D
                       , version        = (1, 0)
                       , shouldInit     = true
                       , implementation = topGD (rbus uart_1)
                                                (dinputs [ in_pb_7
                                                         , in_pb_5
                                                         , in_pb_8
                                                         , in_pb_4
                                                         ]
                                                )
                                                (sht21 i2c_0)
                                                npx_pwm_1
                       }
                                                -- vibro out_pa_1