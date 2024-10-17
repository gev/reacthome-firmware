{-# LANGUAGE NumericUnderscores #-}
module Formula.Smart.Top.TopA4T where

import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F3x0
import           Feature.DInputs              (dinputs)
import           Feature.Sht21                (sht21)
import           Feature.Smart.Top.PowerTouch (powerTouch)
import           Feature.Smart.Top.Vibro      (vibro)
import           Implementation.Smart.TopA4T  (topA4T)
import           Ivory.Language
import           Transport.UART.RBUS          (rbus)

smartTopA4T :: Formula GD32F3x0
smartTopA4T =  Formula { name           = "smart_top_a4t"
                       , model          = deviceTypeSmartTopA4T
                       , version        = (4, 6)
                       , shouldInit     = false
                       , implementation = topA4T (rbus uart_1 115_200)
                                                 (dinputs $  in_pb_4
                                                          :> in_pb_8
                                                          :> in_pb_5
                                                          :> in_pb_7
                                                          :> Nil
                                                 )
                                                 (vibro out_pa_1)
                                                 (powerTouch out_pa_8)
                                                 (sht21 i2c_0)
                                                 npx_pwm_1
                                                 etc
                       }
