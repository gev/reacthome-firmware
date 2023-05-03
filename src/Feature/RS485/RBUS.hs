{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}

module Feature.RS485.RBUS where

import           Control.Monad              (zipWithM)
import           Control.Monad.Reader       (MonadReader, asks)
import           Control.Monad.Writer       (MonadWriter)
import           Core.Context
import           Core.Controller
import qualified Core.Domain                as D
import           Core.Feature
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
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RS485.RBUS
import           Protocol.RS485.RBUS.Master



rbus :: (MonadWriter Context m, MonadReader (D.Domain p t) m, LazyTransport t)
     => [m RS485] -> m Feature
rbus rs485 = do
    let n   = length rs485
    list   <- zipWithM rbus' rs485 [1..]
    pure $ Feature list


rbus' :: (MonadWriter Context m, MonadReader (D.Domain p t) m, LazyTransport t)
     => m RS485 -> Int -> m RBUS
rbus' rs485 index = do
    rs               <- rs485

    mcu              <- asks D.mcu
    transport        <- asks D.transport

    let name          = "feature_rs485_rbus_" <> show index
    let clock         = systemClock mcu

    rs               <- rs485
    isRBUS           <- value  (name <> "_is_rbus"          ) true
    baudrate         <- value  (name <> "_baudrate"         ) defaultBaudrate
    lineControl      <- value  (name <> "_line_control"     ) 0
    rxBuff           <- buffer (name <> "_rx"               )
    rxQueue          <- queue  (name <> "_rx"               )
    msgOffset        <- buffer (name <> "_msg_offset"       )
    msgSize          <- buffer (name <> "_msg_size"         )
    msgConfirm       <- values (name <> "_msg_confirm"      ) (replicate 255 false)
    msgTTL           <- buffer (name <> "_msg_ttl"          )
    msgQueue         <- queue  (name <> "_msg"              )
    msgBuff          <- buffer (name <> "_msg"              )
    msgIndex         <- value  (name <> "_msg_index"        ) 0
    txBuff           <- buffer (name <> "_tx"               )
    rxLock           <- value  (name <> "_rx_lock"          ) false
    txLock           <- value  (name <> "_tx_lock"          ) false
    rxTimestamp      <- value  (name <> "_timestamp_rx"     ) 0
    txTimestamp      <- value  (name <> "_timestamp_tx"     ) 0
    shouldDiscovery  <- value  (name <> "_should_discovery" ) false
    shouldConfirm    <- value  (name <> "_should_confirm"   ) false
    shouldPing       <- value  (name <> "_should_ping"      ) true
    discoveryAddress <- value  (name <> "_address_discovery") broadcastAddress
    confirmAddress   <- value  (name <> "_address_confirm"  ) broadcastAddress
    pingAddress      <- value  (name <> "_address_ping"     ) broadcastAddress

    let onMessage mac address buff n shouldHandle = do
            when shouldHandle $ do
                T.lazyTransmit transport $ \transmit -> do
                    transmit (8 + n)
                    arrayMap $ \ix ->
                        transmit =<< deref (mac ! ix)
                    transmit $ fromIntegral index
                    transmit address
                    for (toIx n) $ \ix ->
                        transmit =<< deref (buff ! ix)
            store confirmAddress address
            store shouldConfirm true

    let onConfirm address = remove msgQueue

    let onPing mac address model version = do
            T.lazyTransmit transport $ \transmit -> do
                transmit 12
                arrayMap $ \ix ->
                    transmit =<< deref (mac ! ix)
                transmit $ fromIntegral index
                transmit address
                transmit 0xf0
                transmit =<< deref model
                transmit =<< deref (version ~> major)
                transmit =<< deref (version ~> minor)

    let onDiscovery address = do
            store discoveryAddress address
            store shouldDiscovery true

    let onReceive = store rxLock false

    protocol <- master name onMessage onConfirm onDiscovery onPing onReceive

    let rbus = RBUS { index, clock, rs, isRBUS, baudrate, lineControl, protocol
                    , rxBuff, rxQueue
                    , msgOffset, msgSize, msgConfirm, msgTTL, msgQueue, msgBuff, msgIndex
                    , txBuff
                    , rxLock, txLock
                    , rxTimestamp, txTimestamp
                    , shouldDiscovery, shouldConfirm, shouldPing
                    , discoveryAddress, confirmAddress, pingAddress
                    }

    addHandler $ HandleRS485 rs (rxHandle rbus) (txHandle rbus)

    let rbusInit :: Def ('[] :-> ())
        rbusInit = proc (name <> "_init") $ body $ do
            configureRS485 rs defaultBaudrate WL_8b SB_1b None

    addInit rbusInit

    addTask $ yeld (name <> "_rx"   ) $ rxTask    rbus
    addTask $ yeld (name <> "_tx"   ) $ txTask    rbus
    addTask $ yeld (name <> "_reset") $ resetTask rbus

    pure rbus



instance Controller [RBUS] where
    handle list buff size =
        pure [ size >=? 9 ==> do
                action <- deref $ buff ! 0
                cond_ [ action ==? 0xa1 ==> transmitRBUS list buff size
                      ]
             ]
