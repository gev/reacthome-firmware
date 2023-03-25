{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}

module Transport.USART.RBUS    where

import           Control.Monad.Reader      (MonadReader, asks)
import           Control.Monad.Writer      (MonadWriter)
import           Core.Context
import           Core.Dispatcher
import qualified Core.Domain               as D
import           Core.Handler
import           Core.Task
import           Core.Transport
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Interface.Mac
import           Interface.MCU             (MCU (peripherals, systemClock), mac)
import           Interface.SystemClock     (getSystemTime)
import           Interface.USART           (HandleUSART (HandleUSART), USART)
import           Ivory.Language
import           Ivory.Stdlib
import qualified Protocol.USART.RBUS       as U
import           Transport.USART.RBUS.Data
import           Transport.USART.RBUS.Rx
import           Transport.USART.RBUS.Tx


rbus :: (MonadWriter Context m, MonadReader (D.Domain p RBUS) m, USART u)
     => (p -> m u) -> m RBUS
rbus usart' = do

    mcu           <- asks D.mcu
    features      <- asks D.features

    let name       = "rbus_usart"
    let clock      = systemClock mcu

    usart         <- usart' $ peripherals mcu
    rxBuff        <- buffer (name <> "_rx")
    rxQueue       <- queue  (name <> "_rx")
    msgOffset     <- buffer (name <> "_msg_offset")
    msgSize       <- buffer (name <> "_msg_size")
    msgQueue      <- queue  (name <> "_msg")
    msgBuff       <- buffer (name <> "_msg")
    msgIndex      <- value  (name <> "_msg_index") 0
    txBuff        <- buffer (name <> "_tx")
    txLock        <- value  (name <> "_tx_lock") false

    {--
        TODO: move dispatcher outside
    --}
    let dispatch = makeDispatcher features

    let onMessage buff n shouldHandle = do
            when shouldHandle $ dispatch buff n


    protocol <- U.rbus name onMessage

    let rbus = RBUS { name, clock, usart, protocol
                    , rxBuff, rxQueue
                    , msgOffset, msgSize, msgQueue, msgBuff, msgIndex
                    , txBuff, txLock
                    }

    addHandler $ HandleUSART usart (rxHandle rbus) (txHandle rbus) Nothing

    addTask $ yeld    (name <> "_rx"  ) $ rxTask rbus
    addTask $ delay 1 (name <> "_tx"  ) $ txTask rbus

    pure rbus

instance Transport RBUS where
    transmit = toQueue
