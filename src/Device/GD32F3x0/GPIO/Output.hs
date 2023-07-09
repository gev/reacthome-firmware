{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Device.GD32F3x0.GPIO.Output where

import           Control.Monad.State
import           Core.Context
import           Device.GD32F3x0.GPIO
import qualified Interface.GPIO.Output        as I
import           Ivory.Language.Module
import           Support.Device.GD32F3x0.GPIO as S


newtype Output = Output {getOutput :: Port}


output :: MonadState Context m => (MODE -> Port) -> m Output
output p = do
    let port = io gpio_mode_output p
    initPort port
    pure $ Output port


instance I.Output Output where
    set   (Output Port{..}) = S.setBit       gpio pin
    get   (Output Port{..}) = S.getOutputBit gpio pin
    reset (Output Port{..}) = S.resetBit     gpio pin
