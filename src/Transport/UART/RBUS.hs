{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}

module Transport.UART.RBUS    where

import           Control.Monad.Reader     (MonadReader, asks)
import           Control.Monad.Writer     (MonadWriter)
import           Core.Context
import           Core.Dispatcher
import qualified Core.Domain              as D
import           Core.Handler
import           Core.Task
import           Core.Transport
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Interface.Mac
import           Interface.MCU            (MCU (peripherals, systemClock), mac)
import           Interface.SystemClock    (getSystemTime)
import           Interface.UART           (HandleUART (HandleUART),
                                           Parity (None), StopBit (SB_1b),
                                           UART (configUART),
                                           WordLength (WL_8b))
import           Ivory.Language
import           Ivory.Stdlib
import qualified Protocol.UART.RBUS       as U
import           Transport.UART.RBUS.Data
import           Transport.UART.RBUS.Rx
import           Transport.UART.RBUS.Tx



rbus :: (MonadWriter Context m, MonadReader (D.Domain p RBUS) m, UART u)
     => (p -> m u) -> m RBUS
rbus uart' = do

    mcu           <- asks D.mcu
    features      <- asks D.features

    let name       = "transport_uart_rbus"
    let clock      = systemClock mcu

    uart         <- uart' $ peripherals mcu
    rxBuff        <- buffer (name <> "_rx"        )
    rxQueue       <- queue  (name <> "_rx"        )
    msgOffset     <- buffer (name <> "_msg_offset")
    msgSize       <- buffer (name <> "_msg_size"  )
    msgQueue      <- queue  (name <> "_msg"       )
    msgBuff       <- buffer (name <> "_msg"       )
    msgIndex      <- value  (name <> "_msg_index" ) 0
    txBuff        <- buffer (name <> "_tx"        )
    txLock        <- value  (name <> "_tx_lock"   ) false

    {--
        TODO: move dispatcher outside
    --}
    let dispatch = makeDispatcher features

    let onMessage buff n shouldHandle = do
            when shouldHandle $ dispatch buff n


    protocol <- U.rbus name onMessage

    let rbus = RBUS { name, clock, uart, protocol
                    , rxBuff, rxQueue
                    , msgOffset, msgSize, msgQueue, msgBuff, msgIndex
                    , txBuff, txLock
                    }

    addHandler $ HandleUART uart (rxHandle rbus) (txHandle rbus) Nothing

    let rbusInit :: Def ('[] :-> ())
        rbusInit = proc (name <> "_init") $ body $ do
            configUART uart 2_000_000 WL_8b SB_1b None

    addInit rbusInit

    addTask $ yeld (name <> "_rx") $ rxTask rbus
    addTask $ yeld (name <> "_tx") $ txTask rbus

    pure rbus

instance Transport RBUS where
    transmitFragment r b = toQueue r b . castDefault . fromIx

instance LazyTransport RBUS where
    lazyTransmit = toQueue'
