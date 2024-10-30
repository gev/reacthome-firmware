{-# LANGUAGE NumericUnderscores #-}
module Formula.Smart.Top.TopG4 where

import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F3x0
import           Feature.DInputs              (dinputs)
import           Feature.Sht21                (sht21)
import           Feature.Smart.Top.PowerTouch (powerTouch)
import           Feature.Smart.Top.Vibro      (vibro)
import           Implementation.Smart.TopG4   (topG4)
import           Ivory.Language
import           Transport.UART.RBUS          (rbus)

smartTopG4 :: Formula GD32F3x0
smartTopG4 =  Formula { name           = "smart_top_g4"
                      , model          = deviceTypeSmartTopG4
                      , version        = (4, 6)
                      , shouldInit     = false
                      , implementation = topG4 (rbus uart_1 115_200)
                                               (dinputs $  in_pb_8
                                                         :> in_pa_15
                                                         :> in_pb_5
                                                         :> in_pb_4
                                                         :> Nil
                                               )
                                               (vibro out_pa_1)
                                               (powerTouch out_pa_8)
                                               (sht21 i2c_0)
                                               npx_pwm_1
                                               etc
                      }