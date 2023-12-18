{-# LANGUAGE FlexibleContexts #-}

module Feature.Smart.Top where

import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State  (MonadState)
import           Core.Context
import           Core.Controller
import qualified Core.Domain          as D
import           Core.Feature
import           Interface.GPIO.Input
import           Interface.MCU
import           Interface.UART



data Top = Top


mkTop :: (MonadState Context m, MonadReader (D.Domain p t) m, UART u, Input i)
      => (p -> m u) -> (p -> m i) -> m Top
mkTop uart' pin' = do
    mcu       <- asks D.mcu
    uart      <- uart' $ peripherals mcu
    pin       <- pin' $ peripherals mcu
    transport <- asks D.transport


    pure Top


top :: (MonadState Context m, MonadReader (D.Domain p t) m, UART u, Input i)
      => (p -> m u) -> (p -> m i) -> m Feature
top uart pin = do
    top <- mkTop uart pin
    pure $ Feature top



instance Controller Top
