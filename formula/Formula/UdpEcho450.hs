{-# LANGUAGE NumericUnderscores #-}

module Formula.UdpEcho450 where

import Control.Monad (void)
import Core.Formula
import Core.Meta
import Device.GD32F4xx
import Implementation.UdpEcho (udpEcho)
import Ivory.Language

udpEcho450 :: Formula GD32F4xx
udpEcho450 =
    Formula
        { meta =
            Meta
                { name = "udpEcho450"
                , model = 0
                , board = 0
                , version = (1, 0)
                , shouldInit = false
                , mcu = gd32f450vgt6
                , quartzFrequency = 25_000_000
                , systemFrequency = 200_000_000
                }
        , implementation = void (udpEcho eth_0)
        }
