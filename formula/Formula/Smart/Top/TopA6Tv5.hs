{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopA6Tv5 where

import Core.Formula
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.Sht21 (sht21)
import Feature.Smart.Top.Vibro (vibro)
import Feature.Touches (touches)
import Implementation.Smart.TopA6Tv5 (topA6Tv5)
import Ivory.Language
import Transport.UART.RBUS
import Device.GD32F3x0.Touch (aluminium)

smartTopA6Tv5 :: Formula GD32F3x0
smartTopA6Tv5 =
    Formula
        { name = "smart_top_a6t_v5"
        , model = deviceTypeSmartTopA6T
        , version = (4, 8)
        , shouldInit = false
        , mcu = gd32f330k8u6
        , quartzFrequency = 8_000_000
        , systemFrequency = 84_000_000
        , implementation =
            topA6Tv5
                (rbusTop uart_1)
                ( touches aluminium $
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
