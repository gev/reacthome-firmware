{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopA4Tv5 where

import Core.Formula
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Device.GD32F3x0.Touch (aluminum)
import Feature.Sht21 (sht21)
import Feature.Smart.Top.Vibro (vibro)
import Feature.Touches (touches)
import Implementation.Smart.TopA4Tv5 (topA4Tv5)
import Ivory.Language
import Transport.UART.RBUS

smartTopA4Tv5 :: Formula GD32F3x0
smartTopA4Tv5 =
    Formula
        { name = "smart_top_a4t_v5"
        , model = deviceTypeSmartTopA4T
        , version = (4, 13)
        , shouldInit = false
        , mcu = gd32f330k8u6
        , quartzFrequency = 8_000_000
        , systemFrequency = 84_000_000
        , implementation =
            topA4Tv5
                (rbusTop uart_1)
                ( touches aluminum $
                    touch_pa6
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
