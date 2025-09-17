{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopG6I where

import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F3x0
import           Feature.Sht21                (sht21)
import           Feature.Smart.Top.PowerTouch (powerTouch)
import           Feature.Smart.Top.Vibro      (vibro)
import           Feature.Touches              (touches)
import           Implementation.Smart.TopG6I  (topG6I)
import           Interface.RS485
import           Ivory.Language
import           Transport.UART.RBUS

smartTopG6I :: Formula GD32F3x0
smartTopG6I =  Formula { name           = "smart_top_g6i"
                      , model           = deviceTypeSmartTopG6
                      , version         = (4, 8)
                      , shouldInit      = false
                      , mcu             = gd32f330k8u6
                      , quartzFrequency =  8_000_000
                      , systemFrequency = 84_000_000
                      , implementation  = topG6I (rbusTop uart_1)
                                                 (touches 10
                                                       $  touch_pa4
                                                       :> touch_pb2
                                                       :> touch_pa5
                                                       :> touch_pb1
                                                       :> touch_pa7
                                                       :> touch_pb0
                                                       :> Nil
                                                 )
                                                 (vibro out_pb_5)
                                                 (sht21 i2c_0)
                                                 npx_pwm_0
                                                 etc
                      }
