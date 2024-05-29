{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}

module Feature.Smart.Top.PowerTouch where

import           Control.Monad.Reader  (MonadReader, asks)
import           Control.Monad.State   (MonadState)
import           Core.Context
import qualified Core.Domain           as D
import           Core.Task             (delay)
import           Interface.GPIO.Output (Output, reset, set)
import           Interface.GPIO.Port   (Pull, pullNone)
import           Interface.MCU         (peripherals, systemClock)



data PowerTouch = PowerTouch

powerTouch :: (MonadState Context m, MonadReader (D.Domain p c) m, Output o, Pull p d)
           => (p -> d -> m o) -> m PowerTouch
powerTouch output' = do
    mcu             <- asks D.mcu
    let peripherals' = peripherals mcu
    output          <- output' peripherals' $ pullNone peripherals'

    addInit "power_touch_off" $ set output

    addTask $ delay 5_000 "power_touch_on" $ reset output

    pure PowerTouch
