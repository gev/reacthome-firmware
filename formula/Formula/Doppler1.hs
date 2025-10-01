{-# LANGUAGE NumericUnderscores #-}

module Formula.Doppler1 where

import Core.Formula
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.ALED (aled)
import Feature.DInputs
import Feature.Dopplers
import Implementation.Doppler (doppler)
import Interface.RS485
import Ivory.Language
import Transport.RS485.RBUS

doppler1 :: Formula GD32F3x0
doppler1 =
    Formula
        { name = "doppler1"
        , model = deviceTypeDoppler1Di4
        , version = (2, 8)
        , shouldInit = true
        , mcu = gd32f330k8u6
        , quartzFrequency = 8_000_000
        , systemFrequency = 84_000_000
        , implementation =
            doppler
                (rbus $ rs485 uart_1 out_pa_4)
                ( dopplers $
                    adc_pa_7
                        :> Nil
                )
                ( dinputs $
                    in_pa_12
                        :> in_pa_11
                        :> in_pa_10
                        :> in_pa_9
                        :> Nil
                )
                (aled npx_pwm_0 etc)
        }
