{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Interface.OneWire where
    
import Interface.Timer (Timer)
import Interface.GPIO.Output
import Control.Monad.State (MonadState)
import Core.Context
import Control.Monad.Reader (MonadReader, asks)
import qualified Core.Domain as D
import Interface.MCU



data OneWire where
  OneWire :: (Output o, Timer t) 
          =>  { out   :: o
              , timer :: t
              } -> OneWire


oneWire :: (MonadState Context m, MonadReader (D.Domain p t1) m, Timer t, Output o)
       => Int -> (p -> m o) -> (p -> m t) -> m OneWire
oneWire n out' timer' = do
    mcu'  <- asks D.mcu
    out   <- out'   $ peripherals mcu'
    timer <- timer' $ peripherals mcu'
    pure OneWire {out, timer}


class Handler HandleOneWire o => OneWire o where 