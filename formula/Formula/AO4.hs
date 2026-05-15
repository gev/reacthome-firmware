{-# LANGUAGE NumericUnderscores #-}

module Formula.AO4 where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Device.GD32F3x0
import Feature.CBM53D04
import Implementation.AO4 qualified as I
import Interface.RS485
import Ivory.Language
import Transport.RS485.RBUS

ao4'v7 :: DFU GD32F3x0
ao4'v7 =
    DFU
        { meta =
            Meta
                { name = "ao4"
                , model = deviceTypeAo4
                , board = 7
                , version = (2, 2)
                , shouldInit = true
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbus $ rs485 uart_1 out_pa_4
        , implementation =
            I.ao4
                (cbm53d04 spi_0 [1, 0, 2, 3])
        }
