{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Blink (compileBlink) where

import           Ivory.Compile.C.CmdlineFrontend
import           Ivory.Language
import           Support.Device.GD32F3x0

main :: Def ('[] :-> Sint32)
main = proc "main" $ body $ do
  enablePeriphClock RCU_GPIOA
  ret 0

blinkModule :: Module
blinkModule = package "blink" $ do
  inclRCU
  incl main

compileBlink :: IO ()
compileBlink =
  runCompiler
    [blinkModule]
    []
    initialOpts
      { outDir = Just "./build"
      , constFold = True
      }
