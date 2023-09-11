{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Device.GD32F3x0.OneWire where

import           Control.Monad.State
import           Core.Context
import           Device.GD32F3x0.GPIO.Mode
import           Device.GD32F3x0.GPIO.Port
import           Device.GD32F3x0.Timer     (Timer)




data OneWire = OneWire
    { port  :: Port
    , timer :: Timer
    }

mkOneWire :: MonadState Context m
          => (Mode -> Port)
          -> Timer
          -> m OneWire
mkOneWire p timer = do
    let port = p openDrain
    initPort port
    pure $ OneWire { port, timer }
