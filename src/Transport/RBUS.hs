{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Transport.RBUS    where

import           Control.Monad.Reader  (Reader, asks, runReader)
import           Control.Monad.Writer  (WriterT)
import           Core.Context
import           Core.Dispatcher
import qualified Core.Domain           as D
import           Core.Task
import           Core.Transport
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Interface.Mac
import           Interface.MCU         (MCU (peripherals, systemClock), mac)
import           Interface.RS485
import           Interface.SystemClock (getSystemTime)
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RBUS.Slave   (slave)
import           Transport.RBUS.Data
import           Transport.RBUS.Rx
import           Transport.RBUS.Tx


rbus :: Reader p RS485 -> (WriterT Context (Reader (D.Domain p t))) RBUS
rbus rs485 = do

    model         <- asks D.model
    version       <- asks D.version
    mcu           <- asks D.mcu
    shouldInit    <- asks D.shouldInit
    features      <- asks D.features

    let name       = "rbus_slave"
    let clock      = systemClock mcu
    let rs         = runReader rs485 $ peripherals mcu

    rxBuff        <- buffer (name <> "_rx")
    rxQueue       <- queue  (name <> "_rx")
    msgOffset     <- buffer (name <> "_msg_offset")
    msgSize       <- buffer (name <> "_msg_size")
    msgTTL        <- buffer (name <> "_msg_ttl")
    msgQueue      <- queue  (name <> "_msg")
    msgBuff       <- buffer (name <> "_msg")
    msgIndex      <- value  (name <> "_msg_index") 0
    txBuff        <- buffer (name <> "_tx")
    initBuff      <- values (name <> "_init_request") [0xf2]
    txLock        <- value  (name <> "_tx_lock") false
    timestamp     <- value  (name <> "_timestamp") 0
    shouldConfirm <- value  (name <> "_should_confirm") false

    {--
        TODO: move dispatcher outside
    --}
    let dispatch = makeDispatcher features

    let onMessage buff n shouldHandle = do
            store (addrOf timestamp) =<< getSystemTime clock
            when shouldHandle $ dispatch buff n
            store (addrOf shouldConfirm) true

    {-
      TODO: Should make Init request here?
      TODO: Should reset Tx queue when address has changed?
    -}
    let onDiscovery = do
            store (addrOf timestamp) =<< getSystemTime clock
            store (addrOf shouldConfirm) false

    let onConfirm = remove msgQueue

    protocol <- slave name (mac mcu) model version onMessage onConfirm onDiscovery

    let rbus = RBUS { name, clock, rs, protocol
                    , rxBuff, rxQueue
                    , msgOffset, msgSize, msgTTL, msgQueue, msgBuff, msgIndex
                    , txBuff, initBuff, txLock, timestamp
                    , shouldConfirm, shouldInit
                    }
    include rs
    include $ HandleRS485 rs (rxHandle rbus) (txHandle rbus)

    include [ yeld    (name <> "_rx"  ) $ rxTask rbus
            , delay 1 (name <> "_tx"  ) $ txTask rbus
            ]

    pure rbus

instance Transport RBUS where
    transmit = toQueue
