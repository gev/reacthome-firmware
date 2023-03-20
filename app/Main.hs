module Main where

import           Build.Firmware
import           Device.GD32F3x0
import           Formula.Blink
import           Formula.Relay12


main :: IO ()
main = do
    build [ (blink  , "blink"  )
          , (relay12, "relay12")
          ] gd32f330k8u6
