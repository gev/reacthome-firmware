{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Device.GD32F4xx.GPIO.Output where

import           Control.Monad.Writer
import           Core.Context
import           Device.GD32F4xx.GPIO
import qualified Interface.GPIO.Output        as I
import           Ivory.Language.Module
import           Support.Device.GD32F4xx.GPIO as S


newtype Output = Output {getOutput :: Port}


output :: MonadWriter Context m => (MODE -> Port) -> m Output
output p = do
    let port = io GPIO_MODE_OUTPUT p
    addInit $ initPort port
    pure $ Output port


instance I.Output Output where
    set   (Output Port{..}) = S.setBit   gpio pin
    reset (Output Port{..}) = S.resetBit gpio pin
