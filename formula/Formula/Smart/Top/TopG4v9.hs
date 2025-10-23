{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopG4v9 where

import Core.Formula
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.Sht21 (sht21)
import Feature.Smart.Top.Vibro (vibro)
import Feature.Touches (touches)
import Implementation.Smart.TopG4v9 (topG4v9)
import Ivory.Language
import Transport.UART.RBUS

smartTopG4v9 :: Formula GD32F3x0
smartTopG4v9 =
      Formula
            { name = "smart_top_g4_v9"
            , model = deviceTypeSmartTopG4
            , version = (4, 8)
            , shouldInit = false
            , mcu = gd32f330k8u6
            , quartzFrequency = 8_000_000
            , systemFrequency = 84_000_000
            , implementation =
                  topG4v9
                        (rbusTop uart_1)
                        ( touches 5 16.5 $
                              touch_pa0
                                    :> touch_pb1
                                    :> touch_pa6
                                    :> touch_pa7
                                    :> Nil
                        )
                        (vibro out_pb_5)
                        (sht21 i2c_0)
                        npx_pwm_0
                        etc
            }
