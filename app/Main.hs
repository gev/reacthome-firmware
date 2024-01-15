module Main where

import           Build.Compiler
import           Build.Compiler.GCC
import           Build.Firmware
import           Control.Monad.State
import           Device.GD32F3x0
import           Device.GD32F4xx
import           Formula.Blink330
import           Formula.Blink450
import           Formula.DI4
import           Formula.DimmerAC12
import           Formula.DimmerDC12
import           Formula.Echo
import           Formula.Mix6x12
import           Formula.Relay12
import           Formula.RsHub4
import           Formula.Scd40
import           Formula.Server
import           Formula.Sht21
import           Formula.Smart.Bottom.Bottom1
import           Formula.Smart.Bottom.Bottom2
import           Formula.Smart.Top.TopA6P
import           Formula.UdpEcho450


main :: IO ()
main = do
    gcc gd32f330k8u6 [ relay12
                     , dimmerDC12
                     , dimmerAC12
                     , mix6x12
                     , di4
                     , sht21_test
                     , smartBottom1
                     , smartBottom2
                     , smartTopA6P
                     , scd40_test
                    --  , blink330
                     ]
    gcc gd32f450vgt6 [ server
                     , udpEcho450
                     , rsHub4
                    --  , blink450
                    --  , echo
                     ]
