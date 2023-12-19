{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Device.GD32F3x0.GPIO.Output where

import           Control.Monad.State
import           Core.Context
import           Device.GD32F3x0.GPIO.Mode
import           Device.GD32F3x0.GPIO.Port
import qualified Interface.GPIO.Output        as I
import           Ivory.Language.Module
import           Support.Device.GD32F3x0.GPIO as S


newtype Output = Output {getOutput :: Port}


mkOutput :: MonadState Context m => (Mode -> Port) -> m Output
mkOutput p = do
    let port = p output
    initPort port
    pure $ Output port


instance I.Input Output where
    get   (Output Port{..}) = S.getOutputBit gpio pin

instance I.Output Output where
    set   (Output Port{..}) = S.setBit       gpio pin
    reset (Output Port{..}) = S.resetBit     gpio pin
