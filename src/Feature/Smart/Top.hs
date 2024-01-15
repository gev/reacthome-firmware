{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}

module Feature.Smart.Top where

import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State  (MonadState)
import           Core.Actions
import           Core.Context
import qualified Core.Domain          as D
import           Core.Transport
import           Data.Buffer
import           GHC.TypeNats
import           Interface.GPIO.Input
import           Interface.GPIO.Port
import           Interface.MCU
import qualified Interface.UART       as I
import           Ivory.Language
import           Ivory.Stdlib
import           Transport.UART.RBUS



newtype Top = Top
    { onMessage :: forall n s t. KnownNat n => Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
    }


top :: (MonadState Context m, MonadReader (D.Domain p t c) m, I.UART u, Input i, Pull p d, LazyTransport t)
      => (p -> m u) -> (p -> d -> m i) -> m Top
top uart' pin' = do
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
