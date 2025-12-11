{-# LANGUAGE NumericUnderscores #-}

module Formula.MixV where

import Core.Formula
import Core.Models
import Data.Fixed
import Device.GD32F4xx
import Feature.DInputs (dinputs)
import Feature.DS18B20 (ds18b20)
import Feature.Dimmers.ACSimple (dimmersAC)
import Implementation.MixV (mix)
import Interface.RS485
import Ivory.Language
import Transport.RS485.RBUS
import Feature.AOutputs (aoutputs)

mixV :: Formula GD32F4xx
mixV =
    Formula
        { name = "mixV"
        , model = deviceTypeMixV
        , version = (2, 0)
        , shouldInit = true
        , mcu = gd32f450vgt6
        , quartzFrequency = 25_000_000
        , systemFrequency = 200_000_000
        , implementation =
            mix
                (rbus $ rs485 uart_3 out_pc_12)
                ( dinputs $
                    in_pe_11
                        :> in_pe_10
                        :> in_pe_9
                        :> in_pe_8
                        :> in_pd_4
                        :> in_pd_3
                        :> in_pd_2
                        :> in_pd_1
                        :> Nil
                )
                ( aoutputs $
                    dac_pa_4
                        :> dac_pa_5
                        :> Nil
                )
                ( dimmersAC
                    ( pwm_2
                        :> pwm_0
                        :> pwm_1
                        :> Nil
                    )
                        exti_pa_1
                )
                (ds18b20 ow_0 od_pe_14)
        }

{--

transports: [rbus $ rs485 uart_3 out_pc_12]

di's: [pe11, pe10, pe9, pe8, pd4, pd3, pd2, pd1]

dimmers: [pwm_2 fan_in, pwm_0 fan_out, pwm_1 dampers] [a1 zero]

analog: [pa4 fan_in, pa5 fan_out]

1w: pe14

--}
