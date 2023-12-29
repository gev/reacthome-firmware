{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.Smart.Top where

import           Control.Monad.Reader     (MonadReader, asks)
import           Control.Monad.State      (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Controller
import qualified Core.Domain              as D
import           Core.Feature
import           Core.Transport
import           Data.Buffer
import           GHC.TypeNats
import           Interface.GPIO.Input
import           Interface.GPIO.Port
import           Interface.MCU
import qualified Interface.UART           as I
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.UART.RBUS       (RBUS (onMessage), rbus)
import           Transport.UART.RBUS
import           Transport.UART.RBUS.Data (RBUS (uart))



newtype Top = Top
    { onMessage :: forall n s t. KnownNat n => Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
    }


mkTop :: (MonadState Context m, MonadReader (D.Domain p t) m, I.UART u, Input i, Pull p d, LazyTransport t)
      => (p -> m u) -> (p -> d -> m i) -> m Top
mkTop uart' pin' = do
    mcu             <- asks D.mcu
    let peripherals' = peripherals mcu
    uart            <- uart' peripherals'
    pin             <- pin' peripherals' $ pullDown peripherals'

    transportUp     <- asks D.transport
    let transmitUp   = lazyTransmit transportUp

    let onMessage' buff size = transmitUp (size + 1) $ \transmit -> do
            transmit actionSmartTop
            for (toIx size) $ \ix -> transmit =<< deref (buff ! ix)

    transportDown   <- mkRbus "transport_uart_rbus" uart onMessage'
    let transmitDown = lazyTransmit transportDown

    let onMessage buff size = transmitDown (size - 1) $ \transmit -> do
            upTo 1 (toIx size) $ \ix -> transmit =<< deref (buff ! ix)

    pure Top { onMessage }


top :: (MonadState Context m, MonadReader (D.Domain p t) m, I.UART u, Input i, Pull p d, LazyTransport t)
      => (p -> m u) -> (p -> d -> m i) -> m Feature
top uart pin = do
    top <- mkTop uart pin
    pure $ Feature top





instance Controller Top where
    handle Top{..} buff size = do
        action <- deref $ buff ! 0
        pure [ action ==? actionSmartTop ==> onMessage buff size
             ]
