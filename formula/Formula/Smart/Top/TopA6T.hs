{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopA6T where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.DInputs (dinputs)
import Feature.Sht21 (sht21)
import Feature.Smart.Top.PowerTouch (powerTouch)
import Feature.Smart.Top.Vibro (vibro)
import Implementation.Smart.TopA6T (topA6T)
import Ivory.Language
import Transport.UART.RBUS

smartTopA6T :: DFU GD32F3x0
smartTopA6T =
    DFU
        { meta =
            Meta
                { name = "smart_top_a6t"
                , model = deviceTypeSmartTopA6T
                , version = (4, 10)
                , shouldInit = false
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbusTop uart_1
        , implementation =
            topA6T
                ( dinputs $
                    in_pa_15
                        :> in_pb_7
                        :> in_pb_3
                        :> in_pb_6
                        :> in_pb_4
                        :> in_pb_5
                        :> Nil
                )
                (vibro out_pa_4)
                (powerTouch out_pa_8)
                (sht21 i2c_0)
                npx_pwm_1
                etc
        }
