module Main where

import           Control.Monad

import           Ivory.Language

import           Shake
import           Firmware

import           Firmware.Blink
import           Firmware.Scheduler
import           Firmware.USART


build :: [(ModuleDef, String)] -> IO ()
build  ms = mapM_ compile ms >> shake (snd <$> ms)

main :: IO ()
main = build
  [ (blink, "blink")
  , (usart, "usart")
  , (scheduler, "scheduler")
  ]
