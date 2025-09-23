{-# LANGUAGE RecordWildCards #-}

module Device.GD32F4xx.GPIO.Output where

import Control.Monad.State
import Core.Context
import Device.GD32F4xx.GPIO.Mode
import Device.GD32F4xx.GPIO.Port
import qualified Interface.GPIO.Output as I
import Ivory.Language.Module
import Support.Device.GD32F4xx.GPIO as S

newtype Output = Output {getOutput :: Port}

mkOutput :: (MonadState Context m) => (Mode -> GPIO_PUPD -> Port) -> GPIO_PUPD -> m Output
mkOutput p pupd = do
    let port = p output pupd
    initPort port
    pure $ Output port

instance I.Input Output where
    get (Output Port{..}) = S.getOutputBit gpio pin

instance I.Output Output where
    set (Output Port{..}) = S.setBit gpio pin
    reset (Output Port{..}) = S.resetBit gpio pin
