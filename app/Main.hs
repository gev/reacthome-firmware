module Main where

import           Build.Compiler
import           Build.Compiler.GCC
import           Build.Firmware
import           Control.Monad.Writer
import           Device.GD32F3x0
import           Formula.Blink
import           Formula.Relay12


main :: IO ()
main = do
    gcc gd32f330k8u6 [ blink
                     , relay12
                     ]
