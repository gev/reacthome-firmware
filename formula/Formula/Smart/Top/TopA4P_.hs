{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopA4P where

import Core.Formula
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.DInputs (dinputs)
import Feature.Sht21 (sht21)
import Implementation.Smart.TopA4P (topA4P)
import Ivory.Language
import Transport.UART.RBUS

smartTopA4P :: Formula GD32F3x0
smartTopA4P =
    Formula
        { name = "smart_top_a4p"
        , model = deviceTypeSmartTopA4P
        , version = (4, 8)
        , shouldInit = false
        , mcu = gd32f330k8u6
        , quartzFrequency = 8_000_000
        , systemFrequency = 84_000_000
        , implementation =
            topA4P
                (rbusTop uart_0)
                ( dinputs $
                    in_pa_4
                        :> in_pb_2
                        :> in_pa_5
                        :> in_pb_1
                        :> Nil
                )
                (sht21 i2c_0)
                npx_pwm_0
                etc
        }
