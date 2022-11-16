{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Support.Device.GD32F3x0.RCU
  ( RCU_GPIO(..)
  , enablePeriphClock
  , rcuModule
  ) where

import           Ivory.Language

data RCU_GPIO = RCU_GPIOA

rcuGPIOA :: Uint32
rcuGPIOA = extern "RCU_GPIOA" "gd32f3x0_rcu.h"

rcuPeriphClockEnable :: Def ('[Uint32] :-> ())
rcuPeriphClockEnable = importProc "rcu_periph_clock_enable" "gd32f3x0_rcu.h"

fromGPIO :: RCU_GPIO -> Uint32
fromGPIO RCU_GPIOA = rcuGPIOA

enablePeriphClock :: RCU_GPIO -> Ivory eff ()
enablePeriphClock = call_ rcuPeriphClockEnable . fromGPIO

rcuModule :: Module
rcuModule = package "rcu_gd32f3x0" $ do
  inclSym rcuGPIOA
  incl rcuPeriphClockEnable
