module Main where

import           Control.Monad
import           Core.Firmware
import           Core.Formula
import           Core.Shake
import           Formula.Blink
import           Formula.Relay6
import           Ivory.Language


build :: [(ModuleDef, String)] -> IO ()
build  ms = mapM_ compile ms >> shake (snd <$> ms)

main :: IO ()
main = build
  [ (cook blink, "blink")
  , (cook relay6, "relay6")
  ]
