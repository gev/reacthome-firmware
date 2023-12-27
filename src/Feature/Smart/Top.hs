{-# LANGUAGE FlexibleContexts #-}

module Feature.Smart.Top where

import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State  (MonadState)
import           Core.Context
import           Core.Controller
import qualified Core.Domain          as D
import           Core.Feature
import           Interface.GPIO.Input
import           Interface.GPIO.Port
import           Interface.MCU
import           Interface.UART



data Top = Top


mkTop :: (MonadState Context m, MonadReader (D.Domain p t) m, UART u, Input i, Pull p d)
      => (p -> m u) -> (p -> d -> m i) -> m Top
mkTop uart' pin' = do
    mcu       <- asks D.mcu
    let peripherals' = peripherals mcu
    uart      <- uart' peripherals'
    pin       <- pin' peripherals' $ pullDown peripherals'
    transport <- asks D.transport


    pure Top


top :: (MonadState Context m, MonadReader (D.Domain p t) m, UART u, Input i, Pull p d)
      => (p -> m u) -> (p -> d -> m i) -> m Feature
top uart pin = do
    top <- mkTop uart pin
    pure $ Feature top



instance Controller Top
