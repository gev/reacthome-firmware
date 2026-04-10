{-# LANGUAGE NumericUnderscores #-}

module Formula.Mix6x12 where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.DInputs (dinputs)
import Feature.Mix.Indicator (indicator)
import Feature.Relays (relays)
import Implementation.Mix (mix)
import Interface.RS485
import Ivory.Language
import Transport.RS485.RBUS

mix6x12'v6 :: DFU GD32F3x0
mix6x12'v6 =
    DFU
        { meta =
            Meta
                { name = "mix6x12"
                , model = deviceTypeMix6x12Rs
                , board = 6
                , version = (3, 13)
                , shouldInit = true
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbus $ rs485 uart_0 out_pb_2
        , implementation =
            mix
                ( dinputs $
                    in_pa_4
                        :> in_pa_15
                        :> in_pb_3
                        :> in_pb_4
                        :> in_pb_5
                        :> in_pa_12
                        :> in_pa_9
                        :> in_pa_10
                        :> in_pa_8
                        :> in_pb_0
                        :> in_pb_1
                        :> in_pa_11
                        :> Nil
                )
                ( relays $
                    out_pa_0
                        :> out_pa_1
                        :> out_pa_2
                        :> out_pa_3
                        :> out_pa_6
                        :> out_pa_7
                        :> Nil
                )
                (indicator npx_pwm_0 150)
                etc
        }
