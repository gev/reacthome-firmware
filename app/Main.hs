module Main where

import           Build.Compiler
import           Build.Compiler.GCC
import           Build.Firmware
import           Control.Monad.Writer
import           Device.GD32F3x0
import           Device.GD32F4xx
import           Formula.Blink330
import           Formula.Blink450
import           Formula.DimmerDC12
import           Formula.DimmerAC12
import           Formula.Echo
import           Formula.MixA6x6
import           Formula.Relay12
import           Formula.Server


main :: IO ()
main = do
    gcc gd32f330k8u6 [ blink330
                     , relay12
                     , dimmerDC12
                     , dimmerAC12
                     , mixA6x6
                     ]
    gcc gd32f450vgt6 [ blink450
                     , echo
                     , server
                     ]
