module Main where

import           Control.Monad

import           Ivory.Language

import           Firmware
import           Shake

import           Formula.Blink
import           Formula.Scheduler
import           Formula.USART


build :: [(ModuleDef, String)] -> IO ()
build  ms = mapM_ compile ms >> shake (snd <$> ms)

main :: IO ()
main = build
  [ (cook blink, "blink")
  , (cook usart, "usart")
  , (cook scheduler, "scheduler")
  ]
