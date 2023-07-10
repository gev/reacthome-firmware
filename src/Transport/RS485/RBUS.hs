{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}

module Transport.RS485.RBUS    where

import           Control.Monad.Reader      (MonadReader, asks)
import           Control.Monad.State       (MonadState, gets)
import           Core.Context
import           Core.Dispatcher
import qualified Core.Domain               as D
import           Core.Handler
import           Core.Task
import           Core.Transport
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Interface.MCU             (MCU (peripherals, systemClock), mac)
import           Interface.RS485
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RS485.RBUS
import           Protocol.RS485.RBUS.Slave (slave)
import           Transport.RS485.RBUS.Data
import           Transport.RS485.RBUS.Rx
import           Transport.RS485.RBUS.Tx


rbus :: (MonadState Context m, MonadReader (D.Domain p RBUS) m)
     => m RS485 -> m RBUS
rbus rs485 = do

    model         <- asks D.model
    version       <- asks D.version
    mcu           <- asks D.mcu
    mustInit      <- asks D.mustInit
    shouldInit    <- asks D.shouldInit
    features      <- asks D.features

    let name       = "transport_rs485_rbus"
    let clock      = systemClock mcu

    rs            <- rs485
    rxBuff        <- buffer (name <> "_rx"            )
    rxQueue       <- queue  (name <> "_rx"            )
    msgOffset     <- buffer (name <> "_msg_offset"    )
    msgSize       <- buffer (name <> "_msg_size"      )
    msgTTL        <- buffer (name <> "_msg_ttl"       )
    msgQueue      <- queue  (name <> "_msg"           )
    msgBuff       <- buffer (name <> "_msg"           )
    msgIndex      <- value  (name <> "_msg_index"     ) 0
    txBuff        <- buffer (name <> "_tx"            )
    initBuff      <- values (name <> "_init_request"  ) [0xf2]
    rxLock        <- value  (name <> "_rx_lock"       ) false
    txLock        <- value  (name <> "_tx_lock"       ) false
    rxTimestamp   <- value  (name <> "_timestamp_rx"  ) 0
    txTimestamp   <- value  (name <> "_timestamp_tx"  ) 0
    initTimestamp <- value  (name <> "_timestamp_init") 0
    shouldConfirm <- value  (name <> "_should_confirm") false

    {--
        TODO: move dispatcher outside
    --}
    let dispatch = makeDispatcher features

    let onMessage buff n shouldHandle = do
            when (n >? 0 .&& shouldHandle) $ dispatch buff n
            store shouldConfirm true

    syncs <- gets getSyncs
    {-
      TODO: Should make Init request here?
      TODO: Should reset Tx queue when address has changed?
    -}
    let onDiscovery = do
         store shouldConfirm false
         store shouldInit mustInit
         mapM_ call_ syncs

    let onConfirm = remove msgQueue

    let onReceive = store rxLock false

    protocol <- slave name (mac mcu) model version onMessage onConfirm onDiscovery onReceive

    let rbus = RBUS { clock, rs, protocol
                    , rxBuff, rxQueue
                    , msgOffset, msgSize, msgTTL, msgQueue, msgBuff, msgIndex
                    , txBuff, initBuff
                    , rxLock, txLock
                    , rxTimestamp, txTimestamp, initTimestamp
                    , shouldConfirm, shouldInit
                    }

    addHandler $ HandleRS485 rs (rxHandle rbus) (txHandle rbus)

    addInit name $ configureRS485 rs defaultBaudrate WL_8b SB_1b None

    addTask $ yeld       (name <> "_rx"   ) $ rxTask    rbus
    addTask $ yeld       (name <> "_tx"   ) $ txTask    rbus
    addTask $ yeld       (name <> "_reset") $ resetTask rbus
    addTask $ delay 2000 (name <> "_init" ) $ initTask  rbus
    {-
        TODO: Move initialization out of the RBUS protocol
    -}

    pure rbus



instance Transport RBUS where
    transmitBuffer = toQueue
