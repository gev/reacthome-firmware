{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Device.GD32F4xx.GPIO.OpenDrain where

import           Control.Monad.State
import           Core.Context
import           Device.GD32F4xx.GPIO.Mode
import           Device.GD32F4xx.GPIO.Port
import qualified Interface.GPIO.OpenDrain     as I
import           Ivory.Language.Module
import           Support.Device.GD32F4xx.GPIO as S


newtype OpenDrain = OpenDrain {getOpenDrain :: Port}


mkOpenDrain :: MonadState Context m => (Mode -> Port) -> m OpenDrain
mkOpenDrain p = do
    let port = p openDrain
    initPort port
    pure $ OpenDrain port


instance I.OpenDrain OpenDrain where
    set   (OpenDrain Port{..}) = S.setBit       gpio pin
    get   (OpenDrain Port{..}) = S.getInputBit  gpio pin
    reset (OpenDrain Port{..}) = S.resetBit     gpio pin
