{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}

module Transport.RS485.RBUS    where

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
import           Interface.RS485
import           Interface.SystemClock     (getSystemTime)
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RS485.RBUS.Slave (slave)
import           Transport.RS485.RBUS.Data
import           Transport.RS485.RBUS.Rx
import           Transport.RS485.RBUS.Tx


rbus :: (MonadWriter Context m, MonadReader (D.Domain p RBUS) m)
     => m RS485 -> m RBUS
rbus rs485 = do

    model         <- asks D.model
    version       <- asks D.version
    mcu           <- asks D.mcu
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
    txLock        <- value  (name <> "_tx_lock"       ) false
    timestamp     <- value  (name <> "_timestamp"     ) 0
    timestamp'    <- value  (name <> "_timestamp_"    ) 0
    shouldConfirm <- value  (name <> "_should_confirm") false

    {--
        TODO: move dispatcher outside
    --}
    let dispatch = makeDispatcher features

    let onMessage buff n shouldHandle = do
            store timestamp =<< getSystemTime clock
            when shouldHandle $ dispatch buff n
            store shouldConfirm true

    {-
      TODO: Should make Init request here?
      TODO: Should reset Tx queue when address has changed?
    -}
    let onDiscovery = do
            store timestamp =<< getSystemTime clock
            store shouldConfirm false

    let onConfirm = remove msgQueue

    protocol <- slave name (mac mcu) model version onMessage onConfirm onDiscovery

    let rbus = RBUS { clock, rs, protocol
                    , rxBuff, rxQueue
                    , msgOffset, msgSize, msgTTL, msgQueue, msgBuff, msgIndex
                    , txBuff, initBuff, txLock, timestamp, timestamp'
                    , shouldConfirm, shouldInit
                    }

    addHandler $ HandleRS485 rs (rxHandle rbus) (txHandle rbus)

    let rbusInit :: Def ('[] :-> ())
        rbusInit = proc (name <> "_init") $ body $ do
            configureRS485 rs 1_000_000 WL_8b SB_1b None

    addInit rbusInit

    addTask $ yeld    (name <> "_rx") $ rxTask rbus
    addTask $ delay 1 (name <> "_tx") $ txTask rbus

    pure rbus

instance Transport RBUS where
    transmitFragment r b = toQueue r b . castDefault . fromIx
