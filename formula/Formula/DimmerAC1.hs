{-# LANGUAGE NumericUnderscores #-}

module Formula.DimmerAC1 where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.Dimmers.AC
import Implementation.Dimmer (dimmer)
import Interface.RS485
import Ivory.Language
import Transport.RS485.RBUS

dimmerAC1 :: DFU GD32F3x0
dimmerAC1 =
    DFU
        { meta =
            Meta
                { name = "dimmerAC1"
                , model = deviceTypeDim1AcRs
                , version = (2, 4)
                , shouldInit = true
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbus $ rs485 uart_1 out_pa_4
        , implementation =
            dimmer
                ( dimmersAC
                    ( pwm_11
                        :> Nil
                    )
                    exti_pb_7
                )
        }
