{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}
{-# LANGUAGE ExistentialQuantification #-}

module Feature.RS485.RSM where

import           Control.Monad.Reader                (MonadReader, asks)
import           Control.Monad.State                 (MonadState)
import           Core.Actions
import           Core.Context
import qualified Core.Domain                         as D
import           Core.Handler
import           Core.Task
import           Core.Transport                      as T
import           Core.Version
import           Data.Buffer
import           Data.Concurrent.Queue               as Q
import           Data.Fixed
import           Data.Serialize
import           Data.Value
import           Feature.RS485.RBUS.Rx
import           Feature.RS485.RBUS.Tx
import           GHC.TypeNats
import           Interface.MCU                       (MCU (peripherals, systemClock))
import qualified Interface.RS485                     as I
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RS485.RBUS                 hiding (message)
import           Protocol.RS485.RBUS.Master          as P
import           Protocol.RS485.RBUS.Master.MacTable as T
import           Protocol.RS485.RBUS.Master.Rx
import           Interface.SystemClock               (SystemClock)
import           Interface.RS485                     (RS485)



data RSM = forall t. (LazyTransport t, Transport t) => RSM
     { index            :: Int
     , clock            :: SystemClock
     , rs               :: RS485
     , baudrate         :: Value      Uint32
     , lineControl      :: Value      Uint8
     , rxBuff           :: Buffer  64 Uint16
     , rxQueue          :: Queue   64
     , msgOffset        :: Buffer  32 Uint16
     , msgSize          :: Buffer  32 Uint16
     , msgTTL           :: Buffer  32 Uint8
     , msgQueue         :: Queue   32
     , msgConfirm       :: Values 255 IBool
     , msgBuff          :: Buffer 512 Uint16
     , msgIndex         :: Value      Uint16
     , txBuff           :: Buffer 255 Uint16
     , rsBuff           :: Buffer 253 Uint8
     , rsSize           :: Value      Uint8
     , rxLock           :: Value      IBool
     , txLock           :: Value      IBool
     , rxTimestamp      :: Value      Uint32
     , txTimestamp      :: Value      Uint32
     , shouldInit       :: Value      IBool
     , synced           :: Value      IBool
     , payload          :: Buffer   8 Uint8
     , transport        :: t
     }



rsm :: (MonadState Context m, MonadReader (D.Domain p c) m, LazyTransport t, Transport t)
     => List n (m I.RS485) -> t -> m (List n RSM)
rsm rs485 transport = zipWithM (rsm' transport) rs485 nats


rsm' :: (MonadState Context m, MonadReader (D.Domain p c) m, LazyTransport t, Transport t)
     => t -> m I.RS485 -> Int -> m RSM
rsm' transport rs485 index = do
    rs               <- rs485

    mcu              <- asks D.mcu
    shouldInit       <- asks D.shouldInit

    let name          = "feature_rs485_rsm_" <> show index
    let clock         = systemClock mcu

    rs               <- rs485
    baudrate         <- value  (name <> "_baudrate"         ) 9600
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
    synced           <- value  (name <> "_synced"           ) false
    payload          <- buffer (name <> "_payload"          )


    let onReceive = store rxLock false


    let rsm = RSM { index, clock, rs, baudrate, lineControl
                    , rxBuff, rxQueue
                    , msgOffset, msgSize, msgConfirm, msgTTL, msgQueue, msgBuff, msgIndex
                    , txBuff
                    , rsBuff, rsSize
                    , rxLock, txLock
                    , rxTimestamp, txTimestamp
                    , shouldInit
                    , synced
                    , payload
                    , transport
                    }

    -- addHandler $ I.HandleRS485 rs (rxHandle rsm) (txHandle rsm)

    -- addTask $ yeld (name <> "_rx"   ) $ rxTask    rsm
    -- addTask $ yeld (name <> "_tx"   ) $ txTask    rsm
    -- addTask $ yeld (name <> "_reset") $ resetTask rsm
    -- addTask $ yeld (name <> "_sync" ) $ syncTask  rsm

    addSync (name <> "_sync") $ forceSyncRSM rsm

    pure rsm



forceSyncRSM :: RSM -> Ivory eff ()
forceSyncRSM RSM{..} = store synced false


forceSyncRSM' :: List n RSM -> Ivory eff ()
forceSyncRSM' = mapM_ forceSyncRSM


syncTask :: RSM -> Ivory (ProcEffects s ()) ()
syncTask r@RSM{..} = do
    shouldInit' <- deref shouldInit
    synced'     <- deref synced
    when (iNot shouldInit' .&& iNot synced') $ do
        T.transmitBuffer transport =<< message r
        store synced true



message :: RSM -> Ivory eff (Buffer 8 Uint8)
message RSM{..} = do
    pack   payload 0 actionRs485Mode
    pack   payload 1 (fromIntegral index :: Uint8)
    packLE payload 3 =<< deref baudrate
    pack   payload 7 =<< deref lineControl
    pure   payload



setMode :: (KnownNat n, KnownNat l)
        => List n RSM
        -> Buffer l Uint8
        -> Uint8
        -> Ivory eff ()
setMode list buff size = do
    when (size ==? 8) $ do
        port <- deref $ buff ! 1
        let run r@RSM{..} p = do
                shouldInit' <- deref shouldInit
                when (iNot shouldInit' .&& p ==? port) $ do
                    store baudrate    =<< unpackLE buff 3
                    store lineControl =<< unpack   buff 7
                    configureMode r
                    store synced false
        zipWithM_ run list $ fromIntegral <$> nats


transmitRS485 :: (KnownNat n, KnownNat l)
              => List n RSM
              -> Buffer l Uint8
              -> Uint8
              -> Ivory (ProcEffects s t) ()
transmitRS485 list buff size = do
    when (size >? 2) $ do
        port <- deref $ buff ! 1
        let run r@RSM{..} p = do
                shouldInit' <- deref shouldInit
                when (iNot shouldInit' .&& p ==? port) $ do
                    let size' = size - 2
                    for (toIx size') $ \ix ->
                        store (txBuff ! toIx (fromIx ix)) . safeCast =<< deref (buff ! (ix + 2))
                    rsTransmit r $ safeCast size'
        zipWithM_ run list $ fromIntegral <$> nats


initialize :: (KnownNat n, KnownNat l)
           => List n RSM
           -> Buffer l Uint8
           -> Uint8
           -> Ivory (ProcEffects s t) ()
initialize list buff size =
    when (size ==? 25) $ do
        let run r@RSM{..} offset = do
                store baudrate    =<< unpackLE buff (offset + 1)
                store lineControl =<< unpack   buff (offset + 5)
                configureMode r
                store shouldInit false
        zipWithM_ run list $ fromIntegral <$> fromList [1, 7..]



configureMode :: RSM -> Ivory eff ()
configureMode r = do
    configureRS485 r      
    store (rxLock r) false
    Q.clear $ rxQueue r



configureRS485 :: RSM -> Ivory eff ()
configureRS485 RSM{..} = do
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

onGetState :: [RSM] -> Ivory eff ()
onGetState = mapM_ run
    where
        run r@RSM{..} = do
            shouldInit' <- deref shouldInit
            when (iNot shouldInit') $ forceSyncRSM r


{-
    TODO: handle actions only when initialized
-}