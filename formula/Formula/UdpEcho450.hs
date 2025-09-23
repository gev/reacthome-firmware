{-# LANGUAGE NumericUnderscores #-}

module Formula.UdpEcho450 where

import Core.Formula
import Device.GD32F4xx
import Implementation.UdpEcho (udpEcho)
import Ivory.Language
import Transport.UART.RBUS

udpEcho450 :: Formula GD32F4xx
udpEcho450 =
    Formula
        { name = "udpEcho450"
        , model = 0xff
        , version = (1, 0)
        , shouldInit = false
        , mcu = gd32f450vgt6
        , quartzFrequency = 24_000_000
        , systemFrequency = 192_000_000
        , implementation = udpEcho eth_0
        }
