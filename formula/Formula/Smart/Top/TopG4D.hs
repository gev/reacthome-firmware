module Formula.Smart.Top.TopG4D where

import           Core.Formula
import           Core.Models
import           Device.GD32F3x0
import           Feature.DInputs          (dinputs)
import           Feature.Sht21            (sht21)
import           Feature.Smart.Top.LEDs   (leds)
import           Implementation.Smart.Top (top)
import           Ivory.Language
import           Transport.UART.RBUS      (rbus)

smartTopG4D :: Formula GD32F3x0
smartTopG4D =  Formula { name           = "smart_top_g4d"
                       , model          = deviceTypeSmartTopA6P
                       , version        = (1, 0)
                       , shouldInit     = true
                       , transport      = rbus uart_1
                       , implementation = top (dinputs [ in_pb_7
                                                       , in_pb_5
                                                       , in_pb_8
                                                       , in_pb_4
                                                       ]
                                              )
                                              (sht21 i2c_0)
                                              (leds npx_pwm_1)
                       }
