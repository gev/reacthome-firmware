{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.RS485.RBUS where

import           Control.Monad                       (zipWithM, zipWithM_)
import           Control.Monad.Reader                (MonadReader, asks)
import           Control.Monad.State                 (MonadState)
import           Core.Context
import           Core.Controller
import qualified Core.Domain                         as D
import           Core.Feature
import           Core.Handler
import           Core.Task
import           Core.Transport                      as T
import           Core.Version
import           Data.Buffer
import           Data.Concurrent.Queue               as Q
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
import           Protocol.RS485.RBUS.Master.Rx



mkRBUS :: (MonadState Context m, MonadReader (D.Domain p t) m, LazyTransport t)
     => [m I.RS485] -> m [RBUS]
mkRBUS rs485 = zipWithM rbus' rs485 [1..]


rbus :: (MonadState Context m, MonadReader (D.Domain p t) m, LazyTransport t)
     => [m I.RS485] -> m Feature
rbus rs485 = Feature <$> mkRBUS rs485


rbus' :: (MonadState Context m, MonadReader (D.Domain p t) m, LazyTransport t)
     => m I.RS485 -> Int -> m RBUS
rbus' rs485 index = do
    rs               <- rs485

    mcu              <- asks D.mcu
    transport        <- asks D.transport

    let name          = "feature_rs485_rbus_" <> show index
    let clock         = systemClock mcu

    rs               <- rs485
    mode             <- value  (name <> "_mode"             ) modeNone
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
    rsBuff           <- buffer (name <> "_rs"               )
    rsSize           <- value  (name <> "_rs_size"          ) 0
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

    let onConfirm address' = peek msgQueue $ \i -> do
            let ix = toIx i
            offset   <- deref $ msgOffset ! ix
            address  <- deref $ msgBuff ! toIx (offset + 1)
            when (address ==? safeCast address') $ remove msgQueue

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

    let onDiscovery mac address model version = do
            onPing mac address model version
            store discoveryAddress address
            store shouldDiscovery true
            store shouldPing false

    let onReceive = store rxLock false

    protocol <- master name onMessage onConfirm onDiscovery onPing onReceive

    let rbus = RBUS { index, clock, rs, mode, baudrate, lineControl, protocol
                    , rxBuff, rxQueue
                    , msgOffset, msgSize, msgConfirm, msgTTL, msgQueue, msgBuff, msgIndex
                    , txBuff
                    , rsBuff, rsSize
                    , rxLock, txLock
                    , rxTimestamp, txTimestamp
                    , shouldDiscovery, shouldConfirm, shouldPing
                    , discoveryAddress, confirmAddress, pingAddress
                    , transport
                    }

    addHandler $ I.HandleRS485 rs (rxHandle rbus) (txHandle rbus)

    addTask $ yeld (name <> "_rx"   ) $ rxTask    rbus
    addTask $ yeld (name <> "_tx"   ) $ txTask    rbus
    addTask $ yeld (name <> "_reset") $ resetTask rbus

    pure rbus


setMode :: KnownNat n
              => [RBUS]
              -> Buffer n Uint8
              -> Uint8
              -> Ivory (ProcEffects s ()) ()
setMode list buff size = do
    when (size ==? 8) $ do
        port <- deref $ buff ! 1
        let run r@RBUS{..} p =
                when (p ==? port) $ do
                    store mode        =<< unpack   buff 2
                    store baudrate    =<< unpackLE buff 3
                    store lineControl =<< unpack   buff 7
                    configureMode r
                    T.lazyTransmit transport $ \transmit -> do
                        transmit 8
                        for 8 $ \ix -> transmit =<< deref (buff ! ix)
        zipWithM_ run list $ fromIntegral <$> [1..]



transmitRBUS :: KnownNat n
             => [RBUS]
             -> Buffer n Uint8
             -> Uint8
             -> Ivory (ProcEffects s ()) ()
transmitRBUS list buff size = do
    when (size >? 9) $ do
        port <- deref $ buff ! 7
        let run r@RBUS{..} p = do
                mode' <- deref mode
                when (mode' ==? modeRBUS .&& p ==? port) $ do
                    address <- deref $ buff ! 8
                    let macTable = P.table protocol
                    lookupMac macTable address $ \rec -> do
                        found <- local $ ival true
                        let mac'  = rec ~> T.mac
                        arrayMap $ \ix -> do
                            m1 <- deref $ mac' ! ix
                            m2 <- deref $ buff ! toIx (1 + fromIx ix)
                            when (m1 /=? m2) $ do
                                store found false
                                breakOut
                        found' <- deref found
                        when found' $
                            toQueue r address buff 9 (size - 9)
        zipWithM_ run list $ fromIntegral <$> [1..]



transmitRB485 :: KnownNat n
              => [RBUS]
              -> Buffer n Uint8
              -> Uint8
              -> Ivory (ProcEffects s ()) ()
transmitRB485 list buff size = do
    when (size >? 2) $ do
        port <- deref $ buff ! 1
        let run r@RBUS{..} p = do
                mode' <- deref mode
                when (mode' ==? modeRS485 .&& p ==? port) $ do
                    let size' = size - 2
                    for (toIx size') $ \ix ->
                        store (txBuff ! toIx (fromIx ix)) . safeCast =<< deref (buff ! (ix + 2))
                    rsTransmit r $ safeCast size'
        zipWithM_ run list $ fromIntegral <$> [1..]



initialize :: KnownNat n
           => [RBUS]
           -> Buffer n Uint8
           -> Uint8
           -> Ivory (ProcEffects s ()) ()
initialize list buff size =
    when (size ==? 25) $ do
        let run r@RBUS{..} offset = do
                store mode        =<< unpack   buff  offset
                store baudrate    =<< unpackLE buff (offset + 1)
                store lineControl =<< unpack   buff (offset + 5)
                configureMode r
        zipWithM_ run list $ fromIntegral <$> [1, 7..]



configureMode :: RBUS -> Ivory eff ()
configureMode r = do
    mode'        <- deref $ mode r
    cond_ [ mode' ==? modeRBUS  ==> configureRBUS  r
          , mode' ==? modeRS485 ==> configureRS485 r
          ]
    store (rxLock r) false
    Q.clear $ rxQueue r



configureRBUS :: RBUS -> Ivory eff ()
configureRBUS RBUS{..} = do
    I.configureRS485 rs defaultBaudrate I.WL_8b I.SB_1b I.None
    store shouldDiscovery false
    store shouldConfirm false
    store shouldPing true
    Q.clear msgQueue
    reset protocol



configureRS485 :: RBUS -> Ivory eff ()
configureRS485 RBUS{..} = do
    baudrate'    <- deref baudrate
    lineControl' <- deref lineControl
    let config lc wl sb p = lineControl' ==? lc
                                         ==> I.configureRS485 rs baudrate' wl sb p
    when (baudrate' >? 0) $
        cond_ [ config 0 I.WL_8b I.SB_1b I.None
              , config 1 I.WL_8b I.SB_1b I.Even
              , config 2 I.WL_8b I.SB_1b I.Odd
              , config 3 I.WL_9b I.SB_1b I.None
              , config 4 I.WL_8b I.SB_2b I.None
              , config 5 I.WL_8b I.SB_2b I.Even
              , config 6 I.WL_8b I.SB_2b I.Odd
              , config 7 I.WL_9b I.SB_2b I.None
              ]
    store rsSize 0



instance Controller [RBUS] where
    handle list buff size =
        pure [ size >? 1 ==> do
                action <- deref $ buff ! 0
                cond_ [ action ==? 0xa0 ==> setMode       list buff size
                      , action ==? 0xa1 ==> transmitRBUS  list buff size
                      , action ==? 0xa2 ==> transmitRB485 list buff size
                      , action ==? 0xf2 ==> initialize    list buff size
                      ]
             ]


{-
    TODO: handle actions only when initialized
-}
