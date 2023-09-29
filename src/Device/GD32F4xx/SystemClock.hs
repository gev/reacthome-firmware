{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}

module Device.GD32F4xx.SystemClock where

import           Control.Monad.State
import           Core.Context
import           Device.GD32F4xx.SysTick       (sysTick)
import           Device.GD32F4xx.Timer         (timer_1)
import           Interface.SystemClock         as I
import           Ivory.Language
import           Support.Device.GD32F4xx.Timer


{--
    TODO:  Use a frequency instead the prescaler
--}

systemClock :: MonadState Context m => m SystemClock
systemClock = I.systemClock (sysTick 199_999)
                            (timer_1 $ timerParam [ prescaler .= ival 99
                                                  , period    .= ival 0xff_ff_ff_ff
                                                  ]
                            )
