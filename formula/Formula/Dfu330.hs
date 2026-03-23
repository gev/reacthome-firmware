{-# LANGUAGE NumericUnderscores #-}

module Formula.Dfu330 where

import Core.Formula
import Core.Models
import Device.GD32F3x0
import Implementation.Dfu
import Interface.RS485
import Ivory.Language
import Transport.RS485.RBUS

dfu330 :: Formula GD32F3x0
dfu330 =
    Formula
        { name = "dfu_330"
        , model = deviceTypeBootloader
        , version = (1, 0)
        , shouldInit = false
        , mcu = gd32f330k8u6
        , quartzFrequency = 8_000_000
        , systemFrequency = 84_000_000
        , implementation =
            dfu
                0x8_002_000
                (rbus $ rs485 uart_1 out_pa_4)
        }