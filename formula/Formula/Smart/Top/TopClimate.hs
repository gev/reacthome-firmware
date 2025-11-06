{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopClimate where

import Core.Formula
import Core.Models
import Device.GD32F3x0
import Feature.Sht21 (sht21)
import Implementation.Smart.TopClimate (topClimate)
import Ivory.Language
import Transport.UART.RBUS

smartTopClimate :: Formula GD32F3x0
smartTopClimate =
    Formula
        { name = "smart_top_climate"
        , model = deviceTypeSmartTopClimate
        , version = (4, 10)
        , shouldInit = false
        , mcu = gd32f330k8u6
        , quartzFrequency = 8_000_000
        , systemFrequency = 84_000_000
        , implementation =
            topClimate
                (rbusTop uart_1)
                (sht21 i2c_0)
        }
