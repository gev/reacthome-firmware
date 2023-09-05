{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Device.GD32F3x0.GPIO.OpenDrain where

import           Control.Monad.State
import           Core.Context
import           Device.GD32F3x0.GPIO
import qualified Interface.GPIO.Input         as I
import qualified Interface.GPIO.Output        as O
import qualified Interface.GPIO.OpenDrain        as OD
import           Ivory.Language.Module
import           Support.Device.GD32F3x0.GPIO as S


newtype OpenDrain = OpenDrain {getOpenDrain :: Port}


openDrain :: MonadState Context m => (MODE -> Port) -> m OpenDrain
openDrain p = do
    let port = p OD
    initPort port
    pure $ OpenDrain port


instance OD.OpenDrain OpenDrain where
    OD.set   (OpenDrain Port{..}) = S.setBit       gpio pin
    OD.get   (OpenDrain Port{..}) = S.getInputBit  gpio pin
    OD.reset (OpenDrain Port{..}) = S.resetBit     gpio pin


instance I.Input OpenDrain where
    I.get  = I.get . getOpenDrain  


instance O.Output OpenDrain where
    O.set    = O.set   . getOpenDrain
    O.get    = O.get   . getOpenDrain
    O.reset  = O.reset . getOpenDrain
