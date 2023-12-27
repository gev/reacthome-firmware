{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Transport.UART.RBUS    where

import           Control.Monad.Reader     (MonadReader, asks)
import           Control.Monad.State      (MonadState)
import           Core.Actions             (actionDiscovery)
import           Core.Context
import           Core.Dispatcher
import qualified Core.Domain              as D
import           Core.Handler
import           Core.Task
import           Core.Transport
import           Core.Version
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import qualified Interface.MCU            as I
import           Interface.UART           (HandleUART (HandleUART),
                                           Parity (None), StopBit (SB_1b),
                                           UART (configUART),
                                           WordLength (WL_8b))
import           Ivory.Language
import qualified Protocol.UART.RBUS       as U
import           Transport.UART.RBUS.Data
import           Transport.UART.RBUS.Rx
import           Transport.UART.RBUS.Tx



rbus :: (MonadState Context m, MonadReader (D.Domain p RBUS) m, UART u)
     => (p -> m u) -> m RBUS
rbus uart' = do

    mcu           <- asks D.mcu
    features      <- asks D.features

    model         <- asks D.model
    version       <- asks D.version
    let mac        = I.mac mcu

    let name       = "transport_uart_rbus"
    let clock      = I.systemClock mcu

    uart          <- uart' $ I.peripherals mcu
    rxBuff        <- buffer (name <> "_rx"          )
    rxQueue       <- queue  (name <> "_rx"          )
    msgOffset     <- buffer (name <> "_msg_offset"  )
    msgSize       <- buffer (name <> "_msg_size"    )
    msgQueue      <- queue  (name <> "_msg"         )
    msgBuff       <- buffer (name <> "_msg"         )
    msgIndex      <- value  (name <> "_msg_index"   ) 0
    txBuff        <- buffer (name <> "_tx"          )
    discoveryBuff <- buffer (name <> "_discovery"   )
    txLock        <- value  (name <> "_tx_lock"     ) false
    rxTimestamp   <- value  (name <> "_timestamp_rx") 0

    {--
        TODO: move dispatcher outside
    --}
    let onMessage = makeDispatcher features

    protocol <- U.rbus name onMessage

    let rbus = RBUS { name
                    , model, version, mac
                    , clock, uart, protocol
                    , rxBuff, rxQueue
                    , msgOffset, msgSize, msgQueue, msgBuff, msgIndex
                    , txBuff
                    , txLock
                    , discoveryBuff
                    , rxTimestamp
                    }

    addHandler $ HandleUART uart (rxHandle rbus) (txHandle rbus) Nothing

    addInit name $ initialize rbus

    addTask $ yeld        (name <> "_rx"       ) $ rxTask        rbus
    addTask $ yeld        (name <> "_tx"       ) $ txTask        rbus
    addTask $ yeld        (name <> "_reset"    ) $ resetTask     rbus
    addTask $ delay 1_000 (name <> "_discovery") $ discoveryTask rbus

    pure rbus



initialize RBUS{..} = do
    store (discoveryBuff ! 0) actionDiscovery
    arrayMap $ \ix -> do
        let jx = toIx $ 1 + fromIx ix
        store (discoveryBuff ! jx) =<< deref (mac ! ix)
    store (discoveryBuff ! 7) =<< deref model
    store (discoveryBuff ! 8) =<< deref (version ~> major)
    store (discoveryBuff ! 9) =<< deref (version ~> minor)
    configUART uart 1_000_000 WL_8b SB_1b None



instance Transport RBUS where
    transmitBuffer = toQueue


instance LazyTransport RBUS where
    lazyTransmit = toQueue'
