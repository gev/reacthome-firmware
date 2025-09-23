module Feature.RS485.RSM where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import qualified Core.Domain as D
import Core.Handler
import Core.Task
import Core.Transport as T
import Core.Version
import Data.Buffer
import Data.Fixed
import Data.Queue as Q
import Data.Serialize
import Data.Value
import Feature.RS485.RSM.Data
import Feature.RS485.RSM.Rx
import Feature.RS485.RSM.Tx
import GHC.TypeNats
import Interface.MCU
import Interface.RS485 (RS485)
import qualified Interface.RS485 as I
import qualified Interface.RS485 as RS
import Interface.SystemClock (SystemClock)
import Ivory.Language
import Ivory.Stdlib

rsm ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , LazyTransport t
    , Transport t
    ) =>
    List n (m (I.RS485 32 300)) ->
    t ->
    m (List n RSM)
rsm rs485 transport = zipWithM (rsm' transport) rs485 nats

rsm' ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , LazyTransport t
    , Transport t
    ) =>
    t ->
    m (I.RS485 32 300) ->
    Int ->
    m RSM
rsm' transport rs485 index = do
    rs <- rs485

    mcu <- asks D.mcu
    shouldInit <- asks D.shouldInit

    let name = "feature_rs485_rsm_" <> show index
    let clock = systemClock mcu

    rs <- rs485
    baudrate <- value (name <> "_baudrate") 9600
    lineControl <- value (name <> "_line_control") 0
    rsBuff <- buffer (name <> "_rs")
    rsSize <- value (name <> "_rs_size") 0
    rxLock <- value (name <> "_rx_lock") false
    txLock <- value (name <> "_tx_lock") false
    rxTimestamp <- value (name <> "_timestamp_rx") 0
    txTimestamp <- value (name <> "_timestamp_tx") 0
    synced <- value (name <> "_synced") false
    payload <- buffer (name <> "_payload")

    let onReceive = store rxLock false

    let rsm =
            RSM
                { index
                , clock
                , rs
                , baudrate
                , lineControl
                , rsBuff
                , rsSize
                , rxLock
                , txLock
                , rxTimestamp
                , txTimestamp
                , shouldInit
                , synced
                , payload
                , transport
                }

    addHandler $
        I.HandleRS485
            rs
            (rxHandle rsm)
            (txHandle rsm)
            (errorHandle rsm)

    addTask $ yeld (name <> "_rx") $ rxTask rsm
    addTask $ yeld (name <> "_sync") $ syncTask rsm

    addSync (name <> "_sync") $ forceSyncRSM rsm

    pure rsm

forceSyncRSM :: RSM -> Ivory eff ()
forceSyncRSM RSM{..} = store synced false

forceSyncRSM' :: List n RSM -> Ivory eff ()
forceSyncRSM' = mapM_ forceSyncRSM

syncTask :: RSM -> Ivory (ProcEffects s ()) ()
syncTask r@RSM{..} = do
    shouldInit' <- deref shouldInit
    synced' <- deref synced
    when (iNot shouldInit' .&& iNot synced') $ do
        T.transmitBuffer transport =<< message r
        store synced true

message :: RSM -> Ivory eff (Buffer 8 Uint8)
message RSM{..} = do
    pack payload 0 actionRs485Mode
    pack payload 1 (fromIntegral index :: Uint8)
    packLE payload 3 =<< deref baudrate
    pack payload 7 =<< deref lineControl
    pure payload

setMode ::
    (KnownNat n, KnownNat l) =>
    List n RSM ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory eff ()
setMode list buff size = do
    when (size ==? 8) $ do
        port <- deref $ buff ! 1
        let run r@RSM{..} p = do
                shouldInit' <- deref shouldInit
                when (iNot shouldInit' .&& p ==? port) $ do
                    store baudrate =<< unpackLE buff 3
                    store lineControl =<< unpack buff 7
                    configureMode r
                    store synced false
        zipWithM_ run list $ fromIntegral <$> nats

transmitRS485 ::
    (KnownNat n, KnownNat l) =>
    List n RSM ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
transmitRS485 list buff size = do
    when (size >? 2) $ do
        port <- deref $ buff ! 1
        let run r@RSM{..} p = do
                shouldInit' <- deref shouldInit
                when (iNot shouldInit' .&& p ==? port) $ do
                    let size' = size - 2
                    RS.transmit rs $ \write ->
                        for (toIx size') $ \ix ->
                            write . safeCast =<< deref (buff ! (ix + 2))
                    store txLock true
        zipWithM_ run list $ fromIntegral <$> nats

initialize ::
    (KnownNat n, KnownNat l) =>
    List n RSM ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
initialize list buff size =
    when (size ==? 25) $ do
        let run r@RSM{..} offset = do
                store baudrate =<< unpackLE buff (offset + 1)
                store lineControl =<< unpack buff (offset + 5)
                configureMode r
                store shouldInit false
        zipWithM_ run list $ fromIntegral <$> fromList [1, 7 ..]

configureMode :: RSM -> Ivory eff ()
configureMode r = do
    configureRS485 r
    store (rxLock r) false
    I.clearRX $ rs r

configureRS485 :: RSM -> Ivory eff ()
configureRS485 RSM{..} = do
    baudrate' <- deref baudrate
    lineControl' <- deref lineControl
    let config lc wl sb p =
            lineControl'
                ==? lc
                ==> I.configureRS485 rs baudrate' wl sb p
    when (baudrate' >? 0) $
        cond_
            [ config 0 I.WL_8b I.SB_1b I.None
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
