
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeOperators      #-}

module Feature.Blink where


import           Device.GPIO
import           Device.MCU.GD32F3x0.GPIO
import           Feature
import           Ivory.Language
import           Ivory.Language.Module        (ModuleM)
import           Support.CMSIS.CoreCM4
import           Support.Device.GD32F3x0.GPIO as GPIO
import           Support.Device.GD32F3x0.RCU


data Blink a = Blink Int a

instance Prepare (Blink (GPIO MCU_GPIO)) where
  prepare (Blink n out) =
     Pack [inclRCU, inclGPIO, inclCoreCM4]
          (prepare' n out)
          (step' n out)

prepare' :: Int -> GPIO MCU_GPIO -> Def ('[] ':-> ())
prepare' n (OUT (PIO (PORT rcu gpio pin (MF mode)))) =
  proc ("blink_" <> show n <> "_init") $ body $ do
    enablePeriphClock rcu
    setMode           gpio mode GPIO_PUPD_NONE pin
    setOutputOptions  gpio GPIO_OTYPE_PP GPIO_OSPEED_50MHZ pin
    retVoid

step' :: Int -> GPIO MCU_GPIO -> Def ('[] ':-> ())
step' n (OUT out) =
  proc ("blink_" <> show n <> "_step") $ body $ do
    nop 10
    setBit' out
    delay 10_000_000
    resetBit' out
    delay 10_000_000


setBit' :: MCU_GPIO -> Ivory eff ()
setBit' (PIO (PORT rcu gpio pin (MF mode))) = GPIO.setBit gpio pin

resetBit' :: MCU_GPIO -> Ivory eff ()
resetBit' (PIO (PORT rcu gpio pin (MF mode))) = GPIO.resetBit gpio pin

delay :: Ix 1_000_000_000 -> Ivory eff ()
delay n = n `times` pure
