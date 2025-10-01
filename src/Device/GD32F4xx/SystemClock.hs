module Device.GD32F4xx.SystemClock where

import Control.Monad.State
import Core.Context
import Device.GD32F4xx.SysTick (sysTick)
import Interface.SystemClock as I

{--
    TODO:  Use a frequency instead the prescaler
--}

systemClock :: (MonadState Context m) => m SystemClock
systemClock = I.systemClock (sysTick 199_999)
