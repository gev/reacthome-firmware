module Main where

import           Build.Compiler
import           Build.Compiler.GCC
import           Build.Firmware
import           Control.Monad.Writer
import           Device.GD32F3x0
import           Device.GD32F4xx
import           Formula.Blink330
import           Formula.Blink450
import           Formula.Relay12


main :: IO ()
main = do
    gcc gd32f330k8u6 [ blink330
                     , relay12
                     ]
    gcc gd32f450vgt6 [ blink450
                     ]
