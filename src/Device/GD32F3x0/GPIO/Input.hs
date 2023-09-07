{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Device.GD32F3x0.GPIO.Input where

import           Control.Monad.State
import           Core.Context
import           Device.GD32F3x0.GPIO.Mode
import           Device.GD32F3x0.GPIO.Port
import qualified Interface.GPIO.Input         as I
import           Ivory.Language
import           Support.Device.GD32F3x0.GPIO as S


newtype Input = Input {getInput :: Port}


mkInput :: MonadState Context m => (Mode -> Port) -> m Input
mkInput p = do
    let port = p input
    initPort port
    pure $ Input port


instance I.Input Input where
    get (Input Port{..}) = S.getInputBit gpio pin
