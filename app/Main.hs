module Main where

import           Build.Compiler
import           Build.Compiler.GCC
import           Build.Firmware
import           Control.Monad.State
import           Device.GD32F3x0
import           Device.GD32F4xx
import           Formula.Blink330
import           Formula.Blink450
import           Formula.DimmerAC12
import           Formula.DimmerDC12
import           Formula.Echo
import           Formula.Mix6x12
import           Formula.Relay12
import           Formula.Server
-- import           Formula.MixA6x12


main :: IO ()
main = do
    gcc gd32f330k8u6 [ relay12
                     , dimmerDC12
                     , dimmerAC12
                     , mix6x12
                    --  , blink330
                     ]
    gcc gd32f450vgt6 [ server
                    --  , blink450
                    --  , echo
                     ]
