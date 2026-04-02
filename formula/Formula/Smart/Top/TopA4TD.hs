{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopA4TD where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.DInputs (dinputs)
import Feature.Sht21 (sht21)
import Feature.Smart.Top.PowerTouch (powerTouch)
import Feature.Smart.Top.Vibro (vibro)
import Implementation.Smart.TopA4TD (topA4TD)
import Ivory.Language
import Transport.UART.RBUS

smartTopA4TD :: DFU GD32F3x0
smartTopA4TD =
    DFU
        { meta =
            Meta
                { name = "smart_top_a4td"
                , model = deviceTypeSmartTopA4TD
                , board = 0
                , version = (4, 10)
                , shouldInit = false
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbusTop uart_1
        , implementation =
            topA4TD
                ( dinputs $
                    in_pb_7
                        :> in_pb_5
                        :> in_pb_8
                        :> in_pb_4
                        :> Nil
                )
                (vibro out_pa_1)
                (powerTouch out_pa_8)
                (sht21 i2c_0)
                npx_pwm_1
                etc
        }
