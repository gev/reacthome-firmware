{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

module Device.GD32F3x0.SystemClock where

import Control.Monad.State
import Core.Context
import Device.GD32F3x0.SysTick (sysTick)
import Device.GD32F3x0.Timer (timer_1, timer_14)
import Interface.SystemClock as I
import Ivory.Language
import Support.Device.GD32F3x0.Timer

{--
    TODO:  Use a frequency instead the prescaler
--}
systemClock :: (MonadState Context m) => m SystemClock
systemClock = I.systemClock (sysTick 83_999)
