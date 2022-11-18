{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Blink (compileBlink) where

import           Ivory.Compile.C.CmdlineFrontend
import           Ivory.Language
import           Support.Device.GD32F3x0         as GD

compileBlink :: IO ()
compileBlink =
  runCompiler
    [blinkModule]
    []
    initialOpts
      { outDir = Just "./build"
      , constFold = True
      }

blinkModule :: Module
blinkModule = package "blink" $ do
  inclRCU
  inclGPIO
  incl nop
  incl delay
  incl main

main :: Def ('[] :-> Sint32)
main = proc "main" $ body $ do

  enablePeriphClock RCU_GPIOA

  setMode           GPIOA
                    GPIO_MODE_OUTPUT
                    GPIO_PUPD_NONE
                    GPIO_PIN_15

  setOutputOptions  GPIOA
                    GPIO_OTYPE_PP
                    GPIO_OSPEED_50MHZ
                    GPIO_PIN_15

  forever $ do
    GD.setBit GPIOA GPIO_PIN_15
    call_ delay 10_000_000
    resetBit GPIOA GPIO_PIN_15
    call_ delay 10_000_000

  ret 0

delay :: Def ('[Ix 1_000_000_000] :-> ())
delay = proc "delay" $ \n -> body $ do
  n `times` const (call_ nop)

nop :: Def ('[] :-> ())
nop = proc "nop" $ body retVoid
