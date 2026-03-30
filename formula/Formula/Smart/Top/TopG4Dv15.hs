{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopG4Dv15 where

import Core.Formula.DFU
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Device.GD32F3x0.Touch (glass)
import Feature.Sht21 (sht21)
import Feature.Smart.Top.Vibro (vibro)
import Feature.Touches (touches)
import Implementation.Smart.TopG4Dv15 (topG4Dv15)
import Ivory.Language
import Transport.UART.RBUS

smartTopG4Dv15 :: DFU GD32F3x0
smartTopG4Dv15 =
    DFU
        { name = "smart_top_g4d_v15"
        , model = deviceTypeSmartTopG4D
        , version = (4, 19)
        , shouldInit = false
        , mcu = gd32f330k8u6
        , quartzFrequency = 8_000_000
        , systemFrequency = 84_000_000
        , transport = rbusTop uart_1
        , implementation =
            topG4Dv15
                ( touches glass $
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
