{-# LANGUAGE NumericUnderscores #-}

module Formula.DimmerAC12 where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.Dimmers.AC
import Feature.Indicator
import Implementation.DimmerDIN (dimmer)
import Interface.RS485
import Ivory.Language
import Transport.RS485.RBUS

dimmerAC12 :: DFU GD32F3x0
dimmerAC12 =
    DFU
        { meta =
            Meta
                { name = "dimmerAC12"
                , model = deviceTypeDim12AcRs
                , board = 0
                , version = (2, 12)
                , shouldInit = true
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbus $ rs485 uart_0 out_pb_2
        , implementation =
            dimmer
                ( dimmersAC
                    ( pwm_11
                        :> pwm_7
                        :> pwm_6
                        :> pwm_8
                        :> pwm_10
                        :> pwm_9
                        :> pwm_0
                        :> pwm_1
                        :> pwm_2
                        :> pwm_3
                        :> pwm_4
                        :> pwm_5
                        :> Nil
                    )
                    exti_pa_5
                )
                (indicator npx_pwm_0 20)
        }
