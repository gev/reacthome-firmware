module Device.GD32F3x0.GPIO.OpenDrain where

import Control.Monad.State
import Core.Context
import Device.GD32F3x0.GPIO.Mode
import Device.GD32F3x0.GPIO.Port
import Interface.GPIO.OpenDrain qualified as I
import Support.Device.GD32F3x0.GPIO as S

newtype OpenDrain = OpenDrain {getOpenDrain :: Port}

mkOpenDrain :: (MonadState Context m) => (Mode -> GPIO_PUPD -> Port) -> m OpenDrain
mkOpenDrain p = do
    let port = p openDrain gpio_pupd_none
    initPort port
    pure $ OpenDrain port

instance I.Input OpenDrain where
    get (OpenDrain Port{..}) = S.getInputBit gpio pin

instance I.Output OpenDrain where
    set (OpenDrain Port{..}) = S.setBit gpio pin
    reset (OpenDrain Port{..}) = S.resetBit gpio pin

instance I.OpenDrain OpenDrain
