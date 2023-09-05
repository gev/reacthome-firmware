module Device.GD32F3x0.OneWire where

import qualified Device.GD32F3x0.GPIO           as G
import           Device.GD32F3x0.Timer

data OneWire = OneWire 
    {   
        pin     :: OpenDrainW
    ,   timer   :: Timer
    }

mkOneWire :: MonadState Context m
          => OpenDrainW
          -> Timer
          -> m OneWire
mkOneWire pin = do
    
    pure OneWire { pin }