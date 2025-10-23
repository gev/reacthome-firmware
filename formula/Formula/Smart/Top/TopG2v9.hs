{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopG2v9 where

import Core.Formula
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.Sht21 (sht21)
import Feature.Smart.Top.Vibro (vibro)
import Feature.Touches (touches)
import Implementation.Smart.TopG2v9 (topG2v9)
import Ivory.Language
import Transport.UART.RBUS

smartTopG2v9 :: Formula GD32F3x0
smartTopG2v9 =
      Formula
            { name = "smart_top_g2_v9"
            , model = deviceTypeSmartTopG2
            , version = (4, 8)
            , shouldInit = false
            , mcu = gd32f330k8u6
            , quartzFrequency = 8_000_000
            , systemFrequency = 84_000_000
            , implementation =
                  topG2v9
                        (rbusTop uart_1)
                        ( touches 5 16.5 $
                              touch_pa1
                                    :> touch_pb0
                                    :> Nil
                        )
                        (vibro out_pb_5)
                        (sht21 i2c_0)
                        npx_pwm_0
                        etc
            }
