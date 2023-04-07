{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
module Device.GD32F3x0.GPIO.Input where

import           Control.Monad.Writer
import           Core.Context
import           Data.Record
import           Device.GD32F3x0.GPIO
import qualified Interface.GPIO.Input         as I
import           Ivory.Language
import           Support.Device.GD32F3x0.GPIO


newtype Input = Input {getInput :: Record Port}




-- input :: MonadWriter Context m => (MODE -> Port) -> m Input
input p = do
    let port = io gpio_mode_input p
    addInit $ initPort port
    pure $ Input port


instance I.Input Input where
    get = undefined
