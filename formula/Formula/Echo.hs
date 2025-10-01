{-# LANGUAGE NumericUnderscores #-}

module Formula.Echo where

import Core.Formula
import Device.GD32F4xx
import Implementation.Echo as E
import Ivory.Language
import Transport.UART.RBUS

echo :: Formula GD32F4xx
echo =
    Formula
        { name = "echo"
        , model = 0xff
        , version = (1, 0)
        , shouldInit = false
        , mcu = gd32f450vgt6
        , quartzFrequency = 8_000_000
        , systemFrequency = 84_000_000
        , implementation = E.echo $ rbusEcho uart_7
        }
