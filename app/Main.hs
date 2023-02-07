module Main where

import           Control.Monad

import           Ivory.Language

import           Firmware
import           Shake

import           Formula
import           Formula.Blink
import           Formula.Relay6


build :: [(ModuleDef, String)] -> IO ()
build  ms = mapM_ compile ms >> shake (snd <$> ms)

main :: IO ()
main = build
  [ (cook blink, "blink")
  , (cook relay6, "relay6")
  ]
