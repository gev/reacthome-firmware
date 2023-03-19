module Main where

import           Build.Shake
import           Core.Firmware
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
