{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Device.GD32F3x0.GPIO.OpenDrain where

import           Control.Monad.State
import           Core.Context
import           Device.GD32F3x0.GPIO
import qualified Interface.GPIO.OpenDrain     as I
import           Ivory.Language.Module
import           Support.Device.GD32F3x0.GPIO as S


newtype OpenDrain = OpenDrain {getOpenDrain :: Port}


openDrain :: MonadState Context m => (MODE -> Port) -> m OpenDrain
openDrain p = do
    let port = p OD
    initPort port
    pure $ OpenDrain port


instance I.OpenDrain OpenDrain where
    set   (OpenDrain Port{..}) = S.setBit       gpio pin
    get   (OpenDrain Port{..}) = S.getInputBit  gpio pin
    reset (OpenDrain Port{..}) = S.resetBit     gpio pin
