{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.RS485.RBUS where

import           Control.Monad                       (zipWithM, zipWithM_)
import           Control.Monad.Reader                (MonadReader, asks)
import           Control.Monad.Writer                (MonadWriter)
import           Core.Context
import           Core.Controller
import qualified Core.Domain                         as D
import           Core.Feature
import           Core.Handler
import           Core.Task
import           Core.Transport                      as T
import           Core.Version
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Serialize
import           Data.Value
import           Feature.RS485.RBUS.Data
import           Feature.RS485.RBUS.Rx
import           Feature.RS485.RBUS.Tx
import           GHC.TypeNats
import           Interface.MCU                       (MCU (peripherals, systemClock))
import qualified Interface.RS485                     as I
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RS485.RBUS
import           Protocol.RS485.RBUS.Master          as P
import           Protocol.RS485.RBUS.Master.MacTable as T



rbus :: (MonadWriter Context m, MonadReader (D.Domain p t) m, LazyTransport t)
     => [m I.RS485] -> m Feature
rbus rs485 = do
    let n   = length rs485
    list   <- zipWithM rbus' rs485 [1..]
    pure $ Feature list


rbus' :: (MonadWriter Context m, MonadReader (D.Domain p t) m, LazyTransport t)
     => m I.RS485 -> Int -> m RBUS
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
                    transmit (9 + n)
                    transmit 0xa1
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
                transmit 13
                transmit 0xa1
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
                    , transport
                    }

    addHandler $ I.HandleRS485 rs (rxHandle rbus) (txHandle rbus)

    let rbusInit :: Def ('[] :-> ())
        rbusInit = proc (name <> "_init") $ body $ do
            I.configureRS485 rs defaultBaudrate I.WL_8b I.SB_1b I.None

    addInit rbusInit

    addTask $ yeld (name <> "_rx"   ) $ rxTask    rbus
    addTask $ yeld (name <> "_tx"   ) $ txTask    rbus
    addTask $ yeld (name <> "_reset") $ resetTask rbus

    pure rbus



configureRS485 :: RBUS -> Ivory eff ()
configureRS485 RBUS{..} = do
    isRBUS'      <- deref isRBUS
    baudrate'    <- deref baudrate
    lineControl' <- deref lineControl
    let config    = I.configureRS485 rs baudrate'
    ifte_ isRBUS'
          (I.configureRS485 rs defaultBaudrate I.WL_8b I.SB_1b I.None)
          (when (baudrate' >? 0) $
              cond_ [ lineControl' ==? 0 ==> config I.WL_8b I.SB_1b I.None
                    -- , lineControl' ==? 1 ==> config I.WL_8b I.SB_1b I.Even
                    -- , lineControl' ==? 2 ==> config I.WL_8b I.SB_1b I.Odd
                    -- , lineControl' ==? 3 ==> config I.WL_9b I.SB_1b I.None
                    -- , lineControl' ==? 4 ==> config I.WL_8b I.SB_2b I.None
                    -- , lineControl' ==? 5 ==> config I.WL_8b I.SB_2b I.Even
                    -- , lineControl' ==? 6 ==> config I.WL_8b I.SB_2b I.Odd
                    -- , lineControl' ==? 7 ==> config I.WL_9b I.SB_2b I.None
                    ]
          )


configureRBUS :: KnownNat n
              => [RBUS]
              -> Buffer n Uint8
              -> Uint8
              -> Ivory (ProcEffects s ()) ()
configureRBUS list buff size = do
    when (size ==? 8) $ do
        port <- deref $ buff ! 1
        let run r@RBUS{..} p =
                when (p ==? port) $ do
                    store isRBUS      =<< unpack   buff 2
                    store baudrate    =<< unpackLE buff 3
                    store lineControl =<< unpack   buff 7
                    configureRS485 r
                    T.lazyTransmit transport $ \transmit -> do
                        transmit 8
                        for 8 $ \ix -> transmit =<< deref (buff ! ix)
        zipWithM_ run list (iterate (+1) 1)



transmitRBUS :: KnownNat n
             => [RBUS]
             -> Buffer n Uint8
             -> Uint8
             -> Ivory (ProcEffects s ()) ()
transmitRBUS list buff size = do
    when (size >? 9) $ do
        port <- deref $ buff ! 7
        let run r@RBUS{..} p = do
                isRBUS' <- deref isRBUS
                when (isRBUS' .&& p ==? port) $ do
                    address <- deref $ buff ! 8
                    let macTable = P.table protocol
                    lookupMac macTable address $ \rec -> do
                        found <- local $ ival true
                        let mac'  = rec ~> T.mac
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



transmitRB485 :: KnownNat n
              => [RBUS]
              -> Buffer n Uint8
              -> Uint8
              -> Ivory (ProcEffects s ()) ()
transmitRB485 list buff size = do
    when (size >? 2) $ do
        port <- deref $ buff ! 1
        let run r@RBUS{..} p = do
                isRBUS' <- deref isRBUS
                when (iNot isRBUS' .&& p ==? port) $ do
                    for (toIx $ size - 2) $ \ix ->
                        store (txBuff ! toIx (fromIx ix)) . safeCast =<< deref (buff ! (ix + 2))
                    rsTransmit r $ safeCast size
        zipWithM_ run list (iterate (+1) 1)



instance Controller [RBUS] where
    handle list buff size =
        pure [ size >? 1 ==> do
                action <- deref $ buff ! 0
                cond_ [ action ==? 0xa0 ==> configureRBUS  list buff size
                      , action ==? 0xa1 ==> transmitRBUS   list buff size
                      , action ==? 0xa2 ==> transmitRB485  list buff size
                      ]
             ]
