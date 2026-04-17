{-# LANGUAGE NumericUnderscores #-}

module Formula.MixF where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.AOutputs (aoutputs)
import Feature.DInputs (dinputs)
import Feature.DS18B20 (ds18b20)
import Feature.Dimmers.DC (dimmersDC)
import Feature.IndicatorFlush (indicator)
import Implementation.MixF (mix)
import Interface.RS485
import Ivory.Language
import Transport.RS485.RBUS

mixF'v2 :: DFU GD32F3x0
mixF'v2 =
    DFU
        { meta =
            Meta
                { name = "mixF"
                , model = deviceTypeMixF
                , board = 2
                , version = (1, 0)
                , shouldInit = true
                , mcu = gd32f350k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbus $ rs485 uart_1 out_pa_5
        , implementation =
            mix
                ( dinputs $
                    in_pa_12
                        :> Nil
                )
                ( aoutputs $
                    dac_pa_4
                        :> Nil
                )
                ( dimmersDC $
                    pwm_5
                        :> pwm_4
                        :> Nil
                )
                (ds18b20 ow_2 od_pa_15)
                (indicator npx_pwm_0 140)
        }
