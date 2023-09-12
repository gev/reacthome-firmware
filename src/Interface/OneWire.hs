{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Interface.OneWire where
    
import Interface.Timer (Timer)
import Interface.GPIO.OpenDrain
import Control.Monad.State (MonadState)
import Core.Context
import Control.Monad.Reader (MonadReader, asks)
import qualified Core.Domain as D
import Interface.MCU
import Ivory.Language



data OneWire where
  OneWire :: (OpenDrain o, Timer t) 
          => { openDrain   :: o
             , timer       :: t
             } -> OneWire

oneWire :: (MonadState Context m, MonadReader (D.Domain p t1) m, Timer t, OpenDrain o)
       => Int -> (p -> m o) -> (p -> m t) -> m OneWire
oneWire n openDrain' timer' = do
    mcu'        <- asks D.mcu
    openDrain   <- openDrain'   $ peripherals mcu'
    timer       <- timer' $ peripherals mcu'
    pure OneWire {openDrain, timer}

class OneWire p => 

-- class Handler HandleOneWire o => OneWire o where 