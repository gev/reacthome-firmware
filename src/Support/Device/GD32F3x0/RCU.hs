{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Support.Device.GD32F3x0.RCU where

import           Ivory.Language

rcuGPIOA :: Uint32
rcuGPIOA = extern "RCU_GPIOA" "gd32f3x0_rcu.h"

rcuPeriphClockEnable :: Def ('[Uint32] :-> ())
rcuPeriphClockEnable = importProc "rcu_periph_clock_enable" "gd32f3x0_rcu.h"
