{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopA6T where

import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F3x0
import           Feature.DInputs              (dinputs)
import           Feature.Sht21                (sht21)
import           Feature.Smart.Top.PowerTouch (powerTouch)
import           Feature.Smart.Top.Vibro      (vibro)
import           Implementation.Smart.TopA6T  (topA6T)
import           Ivory.Language
import           Transport.UART.RBUS          (rbus)

smartTopA6T :: Formula GD32F3x0
smartTopA6T =  Formula { name            = "smart_top_a6t"
                       , model           = deviceTypeSmartTopA6T
                       , version         = (4, 6)
                       , shouldInit      = false
                       , quartzFrequency =  8_000_000
                       , systemFrequency = 84_000_000
                       , implementation  = topA6T (rbus uart_1 115_200)
                                                  (dinputs $  in_pa_15
                                                           :> in_pb_7
                                                           :> in_pb_3
                                                           :> in_pb_6
                                                           :> in_pb_4
                                                           :> in_pb_5
                                                           :> Nil
                                                  )
                                                  (vibro out_pa_4)
                                                  (powerTouch out_pa_8)
                                                  (sht21 i2c_0)
                                                  npx_pwm_1
                                                  etc
                       }
