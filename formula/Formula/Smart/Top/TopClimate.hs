{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopClimate where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Device.GD32F3x0
import Feature.Sht21 (sht21)
import Implementation.Smart.TopClimate (topClimate)
import Ivory.Language
import Transport.UART.RBUS

smartTopClimate :: DFU GD32F3x0
smartTopClimate =
    DFU
        { meta =
            Meta
                { name = "smart_top_climate"
                , model = deviceTypeSmartTopClimate
                , board = 0
                , version = (4, 11)
                , shouldInit = false
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbusTop uart_1
        , implementation =
            topClimate
                (sht21 i2c_0)
        }
