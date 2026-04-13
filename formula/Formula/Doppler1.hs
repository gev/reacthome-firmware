{-# LANGUAGE NumericUnderscores #-}

module Formula.Doppler1 where

import Core.Formula.DFU
import Core.Meta
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

doppler1'v5 :: DFU GD32F3x0
doppler1'v5 =
    DFU
        { meta =
            Meta
                { name = "doppler1"
                , model = deviceTypeDoppler1Di4
                , board = 5
                , version = (2, 11)
                , shouldInit = true
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbus $ rs485 uart_1 out_pa_4
        , implementation =
            doppler
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
