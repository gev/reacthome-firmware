module Main where

import           Control.Monad
import           Firmware
import           Ivory.Language
import           Shake


import           Blink
import           Scheduler
import           USART


build :: (ModuleDef, String) -> IO ()
build  (m, n) = compile m n >> shake n

main :: IO ()
main = mapM_ build
  [ (blink, "blink")
  , (usart, "usart")
  , (scheduler, "scheduler")
  ]

