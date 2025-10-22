{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopG4Dv15 where

import Core.Formula
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.Sht21 (sht21)
import Feature.Smart.Top.Vibro (vibro)
import Implementation.Smart.TopG4Dv15 (topG4Dv15)
import Ivory.Language
import Transport.UART.RBUS
import Feature.Touches (touches)

smartTopG4Dv15 :: Formula GD32F3x0
smartTopG4Dv15 =
    Formula
        { name = "smart_top_g4d_v15"
        , model = deviceTypeSmartTopG4D
        , version = (4, 8)
        , shouldInit = false
        , mcu = gd32f330k8u6
        , quartzFrequency = 8_000_000
        , systemFrequency = 84_000_000
        , implementation =
            topG4Dv15
                (rbusTop uart_1)
                ( touches 15 16.5 $
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
