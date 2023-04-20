{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators      #-}

module Feature.RS485.RBUS where

import           Control.Monad                       (zipWithM, zipWithM_)
import           Control.Monad.Reader                (MonadReader, asks)
import           Control.Monad.Writer                (MonadWriter)
import           Core.Context
import           Core.Controller
import qualified Core.Domain                         as D
import           Core.Feature
import           Core.FSM                            (transit)
import           Core.Handler
import           Core.Task
import           Core.Transport                      as T
import           Core.Version
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Foldable                       (traverse_)
import           Data.Value
import           Feature.RS485.RBUS.Data
import           Feature.RS485.RBUS.Rx
import           Feature.RS485.RBUS.Tx
import           GHC.TypeNats
import           Interface.Mac
import           Interface.MCU                       (MCU (peripherals, systemClock))
import           Interface.RS485
import           Interface.SystemClock               (getSystemTime)
import           Ivory.Language
import           Ivory.Language.Pointer
import           Ivory.Stdlib
import           Protocol.RS485.RBUS                 (broadcastAddress)
import qualified Protocol.RS485.RBUS.Master          as P
import           Protocol.RS485.RBUS.Master.MacTable as M
import           Protocol.RS485.RBUS.Master.Tx       (transmitMessage)



rbus :: (MonadWriter Context m, MonadReader (D.Domain p t) m, LazyTransport t, Transport t)
     => [m RS485] -> m Feature
rbus rs485 = do
    let n   = length rs485
    list   <- zipWithM rbus' rs485 [1..]
    pure $ Feature list


rbus' :: (MonadWriter Context m, MonadReader (D.Domain p t) m, LazyTransport t, Transport t)
     => m RS485 -> Int -> m RBUS
rbus' rs485 index = do
    rs               <- rs485

    mcu              <- asks D.mcu
    transport        <- asks D.transport

    let name          = "feature_rs485_rbus_" <> show index
    let clock         = systemClock mcu

    rs               <- rs485
    rxBuff           <- buffer (name <> "_rx"               )
    rxQueue          <- queue  (name <> "_rx"               )
    msgOffset        <- buffer (name <> "_msg_offset"       )
    msgSize          <- buffer (name <> "_msg_size"         )
    msgTTL           <- buffer (name <> "_msg_ttl"          )
    msgQueue         <- queue  (name <> "_msg"              )
    msgBuff          <- buffer (name <> "_msg"              )
    msgIndex         <- value  (name <> "_msg_index"        ) 0
    txBuff           <- buffer (name <> "_tx"               )
    txLock           <- value  (name <> "_tx_lock"          ) false
    timestamp        <- value  (name <> "_timestamp"        ) 0
    shouldDiscovery  <- value  (name <> "_should_discovery" ) false
    shouldConfirm    <- value  (name <> "_should_confirm"   ) false
    shouldPing       <- value  (name <> "_should_ping"      ) true
    discoveryAddress <- value  (name <> "_address_discovery") broadcastAddress
    confirmAddress   <- value  (name <> "_address_confirm"  ) broadcastAddress
    pingAddress      <- value  (name <> "_address_ping"     ) broadcastAddress

    let onMessage mac address buff n shouldHandle = do
            when shouldHandle $ do
                T.lazyTransmit transport $ \transmit -> do
                    arrayMap $ \ix ->
                        transmit =<< deref (mac ! ix)
                    transmit $ fromIntegral index
                    transmit address
                    transmit n
                    for (toIx n) $ \ix ->
                        transmit =<< deref (buff ! ix)
            store confirmAddress address
            store shouldConfirm true

    let onConfirm = remove msgQueue

    let onPing mac address model version = do
            T.lazyTransmit transport $ \transmit -> do
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

    protocol <- P.master name onMessage onConfirm onDiscovery onPing

    let rbus = RBUS { index, clock, rs, protocol
                    , rxBuff, rxQueue
                    , msgOffset, msgSize, msgTTL, msgQueue, msgBuff, msgIndex
                    , txBuff, txLock, timestamp
                    , shouldDiscovery, shouldConfirm, shouldPing
                    , discoveryAddress, confirmAddress, pingAddress
                    }

    addHandler $ HandleRS485 rs (rxHandle rbus) (txHandle rbus)

    let rbusInit :: Def ('[] :-> ())
        rbusInit = proc (name <> "_init") $ body $ do
            configureRS485 rs 1_000_000 WL_8b SB_1b None

    addInit rbusInit

    addTask $ yeld (name <> "_rx") $ rxTask rbus
    addTask $ yeld (name <> "_tx") $ txTask rbus

    pure rbus



instance Controller [RBUS] where
    handle list buff size =
        pure [ size >=? 9 ==> do
                action <- deref $ buff ! 0
                cond_ [ action ==? 0xa1 ==> transmitRBUS list buff size
                      ]
             ]



transmitRBUS :: KnownNat n
             => [RBUS]
             -> Buffer n Uint8
             -> Uint8
             -> Ivory (ProcEffects s ()) ()
transmitRBUS list buff size = do
    port <- deref $ buff ! 7
    let run r@RBUS{..} p =
            when (p ==? port) $ do
                address <- deref $ buff ! 8
                let macTable = P.table protocol
                lookupMac macTable address $ \rec -> do
                    found <- local $ ival true
                    let mac'  = rec ~> mac
                    arrayMap $ \ix -> do
                        m1 <- deref $ mac' ! ix
                        m2 <- deref $ buff ! toIx (1 + fromIx ix)
                        when (m1 /=? m1) $ do
                            store found false
                            breakOut
                    found' <- deref found
                    when found' $
                        toQueue r address buff 9 (size - 9)
    zipWithM_ run list (iterate (+1) 1)
