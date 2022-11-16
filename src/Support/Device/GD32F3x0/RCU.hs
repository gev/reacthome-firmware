{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Support.Device.GD32F3x0.RCU
  ( RCU_GPIO(..)
  , enablePeriphClock
  , inclRCU
  ) where

import           Ivory.Language
import           Ivory.Language.Module (ModuleM)

data RCU_GPIO = RCU_GPIOA

headerFile = "gd32f3x0_rcu.h"

rcuGPIOA :: Uint32
rcuGPIOA = extern "RCU_GPIOA" headerFile

rcuPeriphClockEnable :: Def ('[Uint32] :-> ())
rcuPeriphClockEnable = importProc "rcu_periph_clock_enable" headerFile

fromGPIO :: RCU_GPIO -> Uint32
fromGPIO RCU_GPIOA = rcuGPIOA

enablePeriphClock :: RCU_GPIO -> Ivory eff ()
enablePeriphClock = call_ rcuPeriphClockEnable . fromGPIO

inclRCU :: ModuleM ()
inclRCU = do
  inclSym rcuGPIOA
  incl rcuPeriphClockEnable
