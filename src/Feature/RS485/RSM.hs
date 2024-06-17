{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Feature.RS485.RSM where

import           Control.Monad.Reader                (MonadReader, asks)
import           Control.Monad.State                 (MonadState)
import           Core.Actions
import           Core.Context
import qualified Core.Domain                         as D
import           Core.Handler
import           Core.Task
import           Core.Transport                      as T
import           Data.Buffer
import           Data.Concurrent.Queue               as Q
import           Data.Fixed
import           Data.Serialize
import           Data.Value
import           Ivory.Language
import           Ivory.Stdlib
import           Interface.RS485
import qualified Interface.UART as IU
import qualified Interface.RS485 as I
import GHC.TypeLits (KnownNat)
import Data.ByteString (index)

data RSM = forall t. (LazyTransport t, Transport t) => RSM
    { index       :: Int
    , rs          :: RS485
    , baudrate    :: Value      Uint32
    , lineControl :: Value      Uint8
    , rxQueue     :: Queue   64
    , rxBuff      :: Buffer  64 Uint16
    , msgOffset   :: Buffer  32 Uint16
    , msgSize     :: Buffer  32 Uint16
    , msgTTL      :: Buffer  32 Uint8
    , msgQueue    :: Queue   32
    , msgConfirm  :: Values 255 IBool
    , msgBuff     :: Buffer 512 Uint16
    , msgIndex    :: Value      Uint16
    , txBuff      :: Buffer 255 Uint16
    , rsBuff      :: Buffer 253 Uint8
    , rsSize      :: Value      Uint8
    , rxLock      :: Value      IBool
    , txLock      :: Value      IBool
    , shouldInit  :: Value      IBool
    , synced      :: Value      IBool
    , payload     :: Buffer   8 Uint8
    , transport   :: t
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

    let name          = "feature_rs485_" <> show index

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
    synced           <- value  (name <> "_synced"           ) false
    payload          <- buffer (name <> "_payload"          )


    let rsm = RSM { index
                  , rs          
                  , baudrate    
                  , lineControl 
                  , rxQueue     
                  , rxBuff      
                  , msgOffset   
                  , msgSize     
                  , msgTTL      
                  , msgQueue    
                  , msgConfirm  
                  , msgBuff     
                  , msgIndex    
                  , txBuff      
                  , rsBuff      
                  , rsSize      
                  , rxLock      
                  , txLock      
                  , shouldInit  
                  , synced      
                  , payload     
                  , transport    
                }

    addHandler $ I.HandleRS485 rs (rxHandle rsm) (txHandle rsm)

    addTask $ yeld (name <> "_rx"   ) $ rxTask    rsm
    addTask $ yeld (name <> "_tx"   ) $ txTask    rsm
    addTask $ yeld (name <> "_reset") $ resetTask rsm
    addTask $ yeld (name <> "_sync" ) $ syncTask  rsm

    addSync (name <> "_sync") $ forceSyncRSM rsm

    pure rsm


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
                store shouldInit false
        zipWithM_ run list $ fromIntegral <$> fromList [1, 7..]


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

rsTransmit :: RSM -> Uint16 -> Ivory (ProcEffects s t) ()
rsTransmit RSM{..} size = do
    let array = toCArray txBuff
    I.transmit rs array size
    store txLock true


rxRS485 :: RSM -> Ivory (ProcEffects s ()) ()
rxRS485 RSM{..} = do
    baudrate' <- deref baudrate
    when (baudrate' >? 0) $ do
        rsSize'   <- deref rsSize

        let dt     = 40_000 ./ baudrate' + 1 -- wait 4 bytes timeout
        pop rxQueue $ \i -> do
            store (rsBuff ! toIx rsSize') . castDefault =<< deref (rxBuff ! toIx i)
            store rsSize $ rsSize' + 1
        lazyTransmit transport (rsSize' + 2) $ \transmit -> do
            transmit 0xa2
            transmit $ fromIntegral index
            for (toIx rsSize') $ \ix ->
                transmit . castDefault =<< deref (rsBuff ! ix)
            store rsSize 0


configureRS485 :: RSM -> Ivory eff ()
configureRS485 RSM{..} = do
    baudrate'    <- deref baudrate
    lineControl' <- deref lineControl
    let config lc wl sb p = lineControl' ==? lc
                                         ==> I.configureRS485 rs baudrate' wl sb p
    when (baudrate' >? 0) $
        cond_ [ config 0 IU.WL_8b IU.SB_1b IU.None
              , config 1 IU.WL_8b IU.SB_1b IU.Even
              , config 2 IU.WL_8b IU.SB_1b IU.Odd
              , config 3 IU.WL_9b IU.SB_1b IU.None
              , config 4 IU.WL_8b IU.SB_2b IU.None
              , config 5 IU.WL_8b IU.SB_2b IU.Even
              , config 6 IU.WL_8b IU.SB_2b IU.Odd
              , config 7 IU.WL_9b IU.SB_2b IU.None
              ]
    store rsSize 0


rxHandle :: RSM -> Uint16 -> Ivory eff ()
rxHandle RSM{..} value = do
    store rxLock true
    push rxQueue $ \i ->
        store (rxBuff ! toIx i) value



rxTask :: RSM -> Ivory (ProcEffects s ()) ()
rxTask = rxRS485


txHandle :: RSM -> Ivory eff ()
txHandle RSM{..} = do
    store rxLock false