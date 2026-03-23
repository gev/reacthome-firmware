{-# LANGUAGE NumericUnderscores #-}

module Formula.Dfu450 where

import Core.Formula
import Core.Models
import Device.GD32F4xx
import Implementation.Dfu
import Ivory.Language
import Transport.UDP.RBUS

dfu450 :: Formula GD32F4xx
dfu450 =
    Formula
        { name = "dfu_450"
        , model = deviceTypeBootloader
        , version = (1, 0)
        , shouldInit = false
        , mcu = gd32f450vgt6
        , quartzFrequency = 25_000_000
        , systemFrequency = 200_000_000
        , implementation =
            dfu
                0x8_008_000
                (rbus eth_0)
        }