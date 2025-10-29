{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopG6v9 where

import Core.Formula
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.Sht21 (sht21)
import Feature.Smart.Top.Vibro (vibro)
import Feature.Touches (touches)
import Implementation.Smart.TopG6v9 (topG6v9)
import Ivory.Language
import Transport.UART.RBUS

smartTopG6v9 :: Formula GD32F3x0
smartTopG6v9 =
      Formula
            { name = "smart_top_g6_v9"
            , model = deviceTypeSmartTopG6
            , version = (4, 9)
            , shouldInit = false
            , mcu = gd32f330k8u6
            , quartzFrequency = 8_000_000
            , systemFrequency = 84_000_000
            , implementation =
                  topG6v9
                        (rbusTop uart_1)
                        ( touches 16.5 $
                              touch_pa0
                                    :> touch_pb1
                                    :> touch_pa1
                                    :> touch_pb0
                                    :> touch_pa6
                                    :> touch_pa7
                                    :> Nil
                        )
                        (vibro out_pb_5)
                        (sht21 i2c_0)
                        npx_pwm_0
                        etc
            }
