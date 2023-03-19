{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}

module Device.GD32F3x0.SystemClock where

import           Control.Monad.Writer
import           Core.Context
import           Device.GD32F3x0.SysTick       (sysTick)
import           Device.GD32F3x0.Timer         (timer_1)
import           Interface.SystemClock         as I
import           Support.Device.GD32F3x0.Timer



systemClock :: MonadWriter Context m => m SystemClock
systemClock = I.systemClock (sysTick 83_999)
                            (timer_1 timerParam { timerPrescaler = 83
                                                , timerPeriod    = 0xff_ff_ff_ff
                                                }
                            )
