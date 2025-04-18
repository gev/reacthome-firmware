{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopG2 where

import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F3x0
import           Feature.DInputs              (dinputs)
import           Feature.Sht21                (sht21)
import           Feature.Smart.Top.PowerTouch (powerTouch)
import           Feature.Smart.Top.Vibro      (vibro)
import           Implementation.Smart.TopG2   (topG2)
import           Ivory.Language
import           Transport.UART.RBUS          

smartTopG2 :: Formula GD32F3x0
smartTopG2 =  Formula { name            = "smart_top_g2"
                      , model           = deviceTypeSmartTopG2
                      , version         = (4, 7)
                      , shouldInit      = false
                      , mcu             = gd32f330k8u6
                      , quartzFrequency =  8_000_000
                      , systemFrequency = 84_000_000
                      , implementation  = topG2 (rbusTop uart_1)
                                                (dinputs $  in_pb_7
                                                         :> in_pb_3
                                                         :> Nil
                                                )
                                                (vibro out_pa_1)
                                                (powerTouch out_pa_8)
                                                (sht21 i2c_0)
                                                npx_pwm_1
                                                etc
                      }
