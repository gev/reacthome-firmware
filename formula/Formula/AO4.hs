{-# LANGUAGE NumericUnderscores #-}

module Formula.AO4 where

import Core.Formula
import Core.Models
import Device.GD32F3x0
import Feature.CBM53D04
import Implementation.AO4 qualified as I
import Interface.RS485
import Ivory.Language
import Transport.RS485.RBUS

ao4 :: Formula GD32F3x0
ao4 =
    Formula
        { name = "ao4"
        , model = deviceTypeAo4
        , version = (2, 0)
        , shouldInit = true
        , mcu = gd32f330k8u6
        , quartzFrequency = 8_000_000
        , systemFrequency = 84_000_000
        , implementation =
            I.ao4
                (rbus $ rs485 uart_1 out_pa_4)
                (cbm53d04 spi_0 [1, 0, 2, 3])
        }
