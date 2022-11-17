{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Blink (compileBlink) where

import           Ivory.Compile.C.CmdlineFrontend
import           Ivory.Language                  (Def, Ix (..), Module,
                                                  Proc ((:->)), Sint32, body,
                                                  forever, incl, package, proc,
                                                  ret, times)
import           Support.Device.GD32F3x0

main :: Def ('[] :-> Sint32)
main = proc "main" $ body $ do
  enablePeriphClock RCU_GPIOA
  setMode GPIOA GPIO_MODE_OUTPUT GPIO_PUPD_NONE GPIO_PIN_15
  setOutputOptions GPIOA GPIO_OTYPE_PP GPIO_SPEED_50MHZ GPIO_PIN_15
  forever $ do
    setBit GPIOA GPIO_PIN_15
    times (ix :: Ix 10_000_000) (const $ setBit GPIOA GPIO_PIN_15)
    resetBit GPIOA GPIO_PIN_15

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
