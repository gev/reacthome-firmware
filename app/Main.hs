module Main where

import           Control.Monad
import           Firmware
import           Ivory.Language
import           Shake


import           Blink
import           Scheduler
import           USART


build :: [(ModuleDef, String)] -> IO ()
build  ms = mapM_ compile ms >> shake (snd <$> ms)

main :: IO ()
main = build
  [ (blink, "blink")
  , (usart, "usart")
  , (scheduler, "scheduler")
  ]

