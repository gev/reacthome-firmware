{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopA4TDv5 where

import Core.Formula
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.Sht21 (sht21)
import Feature.Smart.Top.Vibro (vibro)
import Implementation.Smart.TopA4TDv5 (topA4TDv5)
import Ivory.Language
import Transport.UART.RBUS
import Feature.Touches (touches)
import Device.GD32F3x0.Touch (aluminium)

smartTopA4TDv5 :: Formula GD32F3x0
smartTopA4TDv5 =
    Formula
        { name = "smart_top_a4td_v5"
        , model = deviceTypeSmartTopA4TD7S
        , version = (4, 12)
        , shouldInit = false
        , mcu = gd32f330k8u6
        , quartzFrequency = 8_000_000
        , systemFrequency = 84_000_000
        , implementation =
            topA4TDv5
                (rbusTop uart_1)
                ( touches aluminium $
                    touch_pa6
                        :> touch_pb1
                        :> touch_pa7
                        :> touch_pb0
                        :> Nil
                )
                (vibro out_pa_15)
                (sht21 i2c_0)
                npx_pwm_0
                etc
        }
