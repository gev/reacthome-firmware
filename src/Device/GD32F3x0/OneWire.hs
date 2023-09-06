{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Device.GD32F3x0.OneWire where

import qualified Device.GD32F3x0.GPIO           as G
import           Device.GD32F3x0.Timer          (Timer)
import           Core.Context
import           Control.Monad.State




data OneWire = OneWire 
    {   
        pin     :: G.Port
    ,   timer   :: Timer
    }

mkOneWire :: MonadState Context m
          => G.Port
          -> Timer
          -> m OneWire
mkOneWire pin timer = do
    
    pure OneWire { pin, timer }