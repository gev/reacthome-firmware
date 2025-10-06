{-# LANGUAGE NumericUnderscores #-}

module Formula.Server where

import Core.Formula
import Core.Models
import Data.Fixed
import Device.GD32F4xx
import Feature.ALED (aled)
import Feature.DInputs
import Feature.DS18B20
import Feature.Dimmers.DC
import Feature.Indicator
import Feature.RS485.RBUS as F
import Implementation.Hub (hub)
import Interface.RS485
import Ivory.Language
import Transport.UART.RBUS

server :: Formula GD32F4xx
server =
    Formula
        { name = "server"
        , model = deviceTypeServer
        , version = (5, 9)
        , shouldInit = true
        , mcu = gd32f450vgt6
        , quartzFrequency = 25_000_000
        , systemFrequency = 200_000_000
        , implementation =
            hub
                (rbusHub uart_1)
                ( F.rbus $
                    rs485 uart_5 out_pb_14
                        :> rs485 uart_3 out_pc_12
                        :> rs485 uart_2 out_pb_15
                        :> rs485 uart_0 out_pd_1
                        :> Nil
                )
                ( dimmersDC $
                    pwm_0
                        :> pwm_1
                        :> pwm_2
                        :> Nil
                )
                ( dinputs $
                    in_pa_8
                        :> in_pb_4
                        :> in_pe_3
                        :> in_pe_2
                        :> Nil
                )
                (ds18b20 ow_0 od_pb_3)
                (indicator npx_pwm_0 120)
                (aled npx_pwm_1 etc)
        }
