{-# LANGUAGE NumericUnderscores #-}

module Device.GD32F4xx.SystemClock where

import           Device.GD32F4xx.SysTick       (sysTick)
import           Device.GD32F4xx.Timer         (timer_1)
import           Interface.SystemClock         as I
import           Support.Device.GD32F4xx.Timer



systemClock :: SystemClock
systemClock = I.systemClock (sysTick 83_999)
                            (timer_1 timerParam { timerPrescaler = 83
                                                , timerPeriod = 0xff_ff_ff_ff
                                                }
                            )
