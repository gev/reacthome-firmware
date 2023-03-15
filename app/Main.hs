module Main where

import           Control.Monad
import           Core.Firmware
import           Core.Formula
import           Build.Shake
import           Formula.Blink
import           Formula.Relay12
import           Ivory.Language


build :: [(ModuleDef, String)] -> IO ()
build  ms = mapM_ compile ms >> shake (snd <$> ms)

main :: IO ()
main = build
  [ (cook blink, "blink")
  , (cook relay12, "relay12")
  ]
