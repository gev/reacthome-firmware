module Formula.Smart.Top.TopA6P where

import           Core.Formula
import           Core.Models
import           Device.GD32F3x0
import           Feature.DInputs          (dinputs)
import           Feature.Sht21            (sht21)
import           Feature.Smart.Top.LEDs   (leds)
import           Implementation.Smart.Top (top)
import           Ivory.Language
import           Transport.UART.RBUS      (rbus)

smartTopA6P :: Formula GD32F3x0
smartTopA6P =  Formula { name           = "smart_top_a6p"
                       , model          = deviceTypeSmartTopA6P
                       , version        = (1, 0)
                       , shouldInit     = true
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
                                              (leds npx_pwm_0)
                       }
