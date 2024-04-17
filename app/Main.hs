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
import           Formula.Doppler1
import           Formula.Doppler5
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
import           Formula.Smart.Top.TopG4D
import           Formula.UdpEcho450


main :: IO ()
main = do
    gcc gd32f330k8u6 [ relay12
                     , dimmerDC12
                     , dimmerAC12
                     , mix6x12
                     , di4
                     , smartBottom1
                     , smartBottom2
                     , smartTopA6P
                     , smartTopG4D
                     , doppler1
                     , doppler5
                    --  , scd40_test
                    --  , sht21_test
                    --  , blink330
                     ]
    gcc gd32f450vgt6 [ server
                     , rsHub4
                    --  , udpEcho450
                    --  , blink450
                    --  , echo
                     ]
