{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopA4TD where

import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F3x0
import           Feature.DInputs              (dinputs)
import           Feature.Sht21                (sht21)
import           Feature.Smart.Top.PowerTouch (powerTouch)
import           Feature.Smart.Top.Vibro      (vibro)
import           Implementation.Smart.TopA4TD (topA4TD)
import           Ivory.Language
import           Transport.UART.RBUS        

smartTopA4TD :: Formula GD32F3x0
smartTopA4TD =  Formula { name            = "smart_top_a4td"
                        , model           = deviceTypeSmartTopA4TD
                        , version         = (4, 8)
                        , shouldInit      = false
                        , mcu             = gd32f330k8u6
                        , quartzFrequency =  8_000_000
                        , systemFrequency = 84_000_000
                        , implementation  = topA4TD (rbusTop uart_1)
                                                    (dinputs $  in_pb_7
                                                             :> in_pb_5
                                                             :> in_pb_8
                                                             :> in_pb_4
                                                             :> Nil
                                                    )
                                                    (vibro out_pa_1)
                                                    (powerTouch out_pa_8)
                                                    (sht21 i2c_0)
                                                    npx_pwm_1
                                                    etc
                        }
