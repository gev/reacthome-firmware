{-# LANGUAGE NumericUnderscores #-}

module Formula.Relay12 where

import Core.Formula
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.Indicator (indicator)
import Feature.Relays (relays)
import Implementation.Relay (relay)
import Interface.RS485
import Ivory.Language
import Transport.RS485.RBUS

relay12 :: Formula GD32F3x0
relay12 =
    Formula
        { name = "relay12"
        , model = deviceTypeRelay12Rs
        , version = (3, 8)
        , shouldInit = true
        , mcu = gd32f330k8u6
        , quartzFrequency = 8_000_000
        , systemFrequency = 84_000_000
        , implementation =
            relay
                (rbus $ rs485 uart_0 out_pb_2)
                ( relays $
                    out_pb_1
                        :> out_pa_11
                        :> out_pa_8
                        :> out_pb_0
                        :> out_pa_9
                        :> out_pa_10
                        :> out_pa_0
                        :> out_pa_1
                        :> out_pa_2
                        :> out_pa_3
                        :> out_pa_6
                        :> out_pa_7
                        :> Nil
                )
                (indicator npx_pwm_0 300)
        }
