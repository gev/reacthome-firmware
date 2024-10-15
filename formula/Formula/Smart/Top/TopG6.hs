{-# LANGUAGE NumericUnderscores #-}
module Formula.Smart.Top.TopG6 where

import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F3x0
import           Feature.DInputs              (dinputs)
import           Feature.Sht21                (sht21)
import           Feature.Smart.Top.PowerTouch (powerTouch)
import           Feature.Smart.Top.Vibro      (vibro)
import           Implementation.Smart.TopG6   (topG6)
import           Ivory.Language
import           Transport.UART.RBUS          (rbus)

smartTopG6 :: Formula GD32F3x0
smartTopG6 =  Formula { name           = "smart_top_g6"
                      , model          = deviceTypeSmartTopG6
                      , version        = (4, 5)
                      , shouldInit     = false
                      , implementation = topG6 (rbus uart_1 115_200)
                                               (dinputs $  in_pb_8
                                                        :> in_pa_15
                                                        :> in_pb_7
                                                        :> in_pb_3
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
