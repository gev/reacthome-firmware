{-# LANGUAGE NumericUnderscores #-}

module Device.GD32F3x0.SystemClock where

import           Device.GD32F3x0.SysTick       (sysTick)
import           Device.GD32F3x0.Timer         (timer_1)
import           Interface.SystemClock
import           Support.Device.GD32F3x0.Timer



systemClock :: SystemClock
systemClock = SystemClock
  { scheduleTimer = sysTick 83_999
  , systemTimer = timer_1 timerParam { timerPrescaler = 83
                                     , timerPeriod    = 0xff_ff_ff_ff
                                     }
  }
