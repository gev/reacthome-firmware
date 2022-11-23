{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Blink (blink) where

import           Ivory.Language
import           Ivory.Language.Module
import           Support.CMSIS.CoreCM4
import           Support.Device.GD32F3x0.GPIO as GPIO
import           Support.Device.GD32F3x0.RCU


blink :: ModuleM ()
blink = do
  inclRCU
  inclGPIO
  inclCoreCM4
  incl main


main :: Def ('[] :-> Sint32)
main = proc "main" $ body $ do
  enablePeriphClock RCU_GPIOA
  setMode           GPIOA GPIO_MODE_OUTPUT GPIO_PUPD_NONE GPIO_PIN_15
  setOutputOptions  GPIOA GPIO_OTYPE_PP GPIO_OSPEED_50MHZ GPIO_PIN_15
  nop 10
  forever $ do
    GPIO.setBit GPIOA GPIO_PIN_15
    delay 10_000_000
    resetBit GPIOA GPIO_PIN_15
    delay 10_000_000
  ret 0

delay :: Ix 1_000_000_000 -> Ivory eff ()
delay n = n `times` pure
