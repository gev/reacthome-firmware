module Main where

import           Build.Compiler
import           Build.Compiler.GCC
import           Control.Monad.Writer
import           Device.GD32F3x0
import           Formula.Blink
import           Formula.Relay12


main :: IO ()
main = do
    let (mcu, context) = runWriter gd32f330k8u6
    let config = makeConfig mcu :: GCC
    build [ (blink  , "blink"  )
          , (relay12, "relay12")
          ] mcu context config
