module Main where

import           Control.Monad

import           Ivory.Language

import           Firmware
import           Shake

import           Feature.Blink
import           Firmware.Blink
import           Firmware.Scheduler
import           Firmware.USART


build :: [(ModuleDef, String)] -> IO ()
build  ms = mapM_ compile ms >> shake (snd <$> ms)

main :: IO ()
main = build
  [ (cook blink, "blink")
  , (cook usart, "usart")
  , (scheduler, "scheduler")
  ]
