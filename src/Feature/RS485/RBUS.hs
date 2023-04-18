{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}

module Feature.RS485.RBUS where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Controller
import qualified Core.Domain                as D
import           Core.Feature
import           Core.FSM                   (transit)
import           Core.Handler
import           Core.Task
import           Core.Transport             as T
import           Core.Version
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Feature.RS485.RBUS.Data
import           Feature.RS485.RBUS.Rx
import           Feature.RS485.RBUS.Tx
import           Interface.MCU              (MCU (peripherals, systemClock))
import           Interface.RS485
import           Interface.SystemClock      (getSystemTime)
import           Ivory.Language
import qualified Protocol.RS485.RBUS.Master as P



rbus :: (MonadWriter Context m, MonadReader (D.Domain p t) m, Transport t)
     => [m RS485] -> m Feature
rbus rs485 = do
    let n     = length rs485
    rbus  <- zipWithM rbus' rs485 [1..]
    pure $ Feature rbus



rbus' :: (MonadWriter Context m, MonadReader (D.Domain p t) m, Transport t)
     => m RS485 -> Int -> m RBUS
rbus' rs485 index = do
    rs            <- rs485

    mcu           <- asks D.mcu
    transport     <- asks D.transport

    let name       = "feature_rs485_rbus_" <> show index
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
    txLock        <- value  (name <> "_tx_lock"       ) false
    timestamp     <- value  (name <> "_timestamp"     ) 0
    shouldConfirm <- value  (name <> "_should_confirm") false

    let onMessage buff n shouldHandle = do
            store timestamp =<< getSystemTime clock
            -- when shouldHandle
            store shouldConfirm true

    let onConfirm = remove msgQueue

    let onPing mac address model version = remove msgQueue

    let onDiscovery mac address model version = do
            pure ()

    protocol <- P.master name onMessage onConfirm onDiscovery onPing

    let rbus = RBUS { index, clock, rs, protocol
                    , rxBuff, rxQueue
                    , msgOffset, msgSize, msgTTL, msgQueue, msgBuff, msgIndex
                    , txBuff, txLock, timestamp
                    , shouldConfirm
                    }

    addHandler $ HandleRS485 rs (rxHandle rbus) (txHandle rbus)

    let rbusInit :: Def ('[] :-> ())
        rbusInit = proc (name <> "_init") $ body $ do
            configureRS485 rs 1_000_000 WL_8b SB_1b None

    addInit rbusInit

    addTask $ yeld    (name <> "_rx") $ rxTask rbus
    -- addTask $ delay 1 (name <> "_tx") $ txTask rbus

    pure rbus



instance Controller [RBUS]
