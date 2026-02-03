{-# HLINT ignore "Use for_" #-}
{-# LANGUAGE NondecreasingIndentation #-}

module Feature.RS485.RBUS where

import Control.Monad ((<=<))
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import Core.Domain qualified as D
import Core.Handler
import Core.Task
import Core.Transport as T
import Core.Version
import Data.Buffer
import Data.Fixed
import Data.Queue as Q
import Data.Serialize
import Data.Value
import Endpoint.DMX512 qualified as ED
import Feature.RS485.RBUS.Data
import Feature.RS485.RBUS.Rx
import Feature.RS485.RBUS.Tx
import GHC.TypeNats
import Interface.MCU
import Interface.RS485 qualified as I
import Interface.RS485 qualified as RS
import Ivory.Language
import Ivory.Stdlib
import Protocol.RS485.RBUS hiding (message)
import Protocol.RS485.RBUS.Master as P
import Protocol.RS485.RBUS.Master.MacTable as T
import Protocol.RS485.RBUS.Master.Rx
import Support.CMSIS.CoreCM4 (nop)
import Support.Cast (castFloatToUint16)

rbus ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , LazyTransport t
    , Transport t
    ) =>
    List n (m (I.RS485 300 600)) ->
    t ->
    m (List n RBUS)
rbus rs485 transport = zipWithM (rbus' transport) rs485 nats

rbus' ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , LazyTransport t
    , Transport t
    ) =>
    t ->
    m (I.RS485 300 600) ->
    Int ->
    m RBUS
rbus' transport rs485 index = do
    mcu <- asks D.mcu
    shouldInit <- asks D.shouldInit

    let name = "feature_rs485_rbus_" <> show index
    let clock = systemClock mcu

    rs <- rs485
    dmx512 <- ED.mkDMX512 (name <> "_dmx512") index
    mode <- value (name <> "_mode") modeNone
    baudrate <- value (name <> "_baudrate") defaultBaudrate
    lineControl <- value (name <> "_line_control") 0
    sizeDMX512 <- value (name <> "_size_dmx512") 0
    curSyncDMX512 <- value (name <> "_current_sync_dmx512") 0
    msgWaitingConfirm <- values (name <> "_msg_waiting_confirm") (replicate 255 false)
    msgConfirmed <- values (name <> "_msg_confirmed") (replicate 255 false)
    msgQueue <- queue (name <> "_msg") =<< messages name
    msgBuff <- buffer (name <> "_msg")
    msgIndex <- value (name <> "_msg_index") 0
    rsBuff <- buffer (name <> "_rs")
    rsSize <- value (name <> "_rs_size") 0
    rxLock <- value (name <> "_rx_lock") false
    txLock <- value (name <> "_tx_lock") false
    rxTimestamp <- value (name <> "_timestamp_rx") 0
    txTimestamp <- value (name <> "_timestamp_tx") 0
    shouldDiscovery <- value (name <> "_should_discovery") false
    shouldConfirm <- value (name <> "_should_confirm") false
    shouldPing <- value (name <> "_should_ping") 5
    discoveryAddress <- value (name <> "_address_discovery") broadcastAddress
    confirmAddress <- value (name <> "_address_confirm") broadcastAddress
    pingAddress <- value (name <> "_address_ping") broadcastAddress
    synced <- value (name <> "_synced") false
    payload <- buffer (name <> "_payload")

    let onMessage mac address buff n shouldHandle = do
            when shouldHandle do
                T.lazyTransmit transport (9 + n) \transmit -> do
                    transmit 0xa1
                    arrayMap \ix ->
                        transmit =<< deref (mac ! ix)
                    transmit $ fromIntegral index
                    transmit address
                    for (toIx n) \ix ->
                        transmit =<< deref (buff ! ix)
            store confirmAddress address
            store shouldConfirm true

    let onConfirm address' = do
            let ax = toIx address'
            waitingConfirm' <- deref $ msgWaitingConfirm ! ax
            when waitingConfirm' do
                store (msgConfirmed ! ax) true
                store (msgWaitingConfirm ! ax) false

    let onPing mac address model version = do
            T.lazyTransmit transport 13 \transmit -> do
                transmit 0xa1
                arrayMap \ix ->
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

    let onReceive = store rxLock false

    protocol <- master name onMessage onConfirm onDiscovery onPing onReceive

    let rbus =
            RBUS
                { index
                , clock
                , dmx512
                , rs
                , mode
                , baudrate
                , lineControl
                , protocol
                , sizeDMX512
                , curSyncDMX512
                , msgWaitingConfirm
                , msgConfirmed
                , msgQueue
                , msgBuff
                , msgIndex
                , rsBuff
                , rsSize
                , rxLock
                , txLock
                , rxTimestamp
                , txTimestamp
                , shouldDiscovery
                , shouldConfirm
                , shouldPing
                , discoveryAddress
                , confirmAddress
                , pingAddress
                , shouldInit
                , synced
                , payload
                , transport
                }

    addHandler $ I.HandleRS485 rs (rxHandle rbus) (txHandle rbus) (errorHandle rbus)

    addTask $ yeld (name <> "_rx") $ rxTask rbus
    addTask $ yeld (name <> "_tx") $ txTask rbus
    addTask $ yeld (name <> "_reset") $ resetTask rbus
    addTask $ yeld (name <> "_sync") $ syncTask rbus

    addTask $ delay 40 (name <> "_break_dmx512") $ dmx512Task rbus

    addTask $ yeld (name <> "sync_dmx512") $ syncDMX512Task rbus
    addTask $ delay 1 (name <> "calculate_dmx512") $ calculateDMX512Task rbus

    addSync (name <> "_sync") $ forceSyncRBUS rbus

    pure rbus

forceSyncRBUS :: RBUS -> Ivory eff ()
forceSyncRBUS RBUS{..} = store synced false

forceSyncRBUS' :: List n RBUS -> Ivory eff ()
forceSyncRBUS' = mapM_ forceSyncRBUS

syncTask :: RBUS -> Ivory (ProcEffects s ()) ()
syncTask r@RBUS{..} = do
    shouldInit' <- deref shouldInit
    synced' <- deref synced
    when (iNot shouldInit' .&& iNot synced') do
        T.transmitBuffer transport =<< message r
        store synced true

message :: RBUS -> Ivory eff (Buffer 10 Uint8)
message RBUS{..} = do
    pack payload 0 actionRs485Mode
    pack payload 1 (fromIntegral index :: Uint8)
    pack payload 2 =<< deref mode
    packBE payload 3 =<< deref baudrate
    pack payload 7 =<< deref lineControl
    packBE payload 8 =<< deref sizeDMX512
    pure payload

setMode ::
    (KnownNat n, KnownNat l) =>
    List n RBUS ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory eff ()
setMode list buff size = do
    when (size ==? 10) do
        port <- deref $ buff ! 1
        let run r@RBUS{..} p = do
                shouldInit' <- deref shouldInit
                when (iNot shouldInit' .&& p ==? port) do
                    store mode =<< unpack buff 2
                    store baudrate =<< unpackBE buff 3
                    store lineControl =<< unpack buff 7
                    store sizeDMX512 =<< unpackBE buff 8
                    configureMode r
                    store synced false
        zipWithM_ run list $ fromIntegral <$> nats

transmitRBUS ::
    (KnownNat n, KnownNat l) =>
    List n RBUS ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
transmitRBUS list buff size = do
    when (size >? 9) do
        port <- deref $ buff ! 7
        let run r@RBUS{..} p = do
                shouldInit' <- deref shouldInit
                mode' <- deref mode
                when (iNot shouldInit' .&& mode' ==? modeRBUS .&& p ==? port) do
                    address <- deref $ buff ! 8
                    let macTable = P.table protocol
                    lookupMac macTable address \rec -> do
                        found <- local $ ival true
                        let mac' = rec ~> T.mac
                        arrayMap \ix -> do
                            m1 <- deref $ mac' ! ix
                            m2 <- deref $ buff ! toIx (1 + fromIx ix)
                            when (m1 /=? m2) do
                                store found false
                                breakOut
                        found' <- deref found
                        when found' do
                            toQueue r address buff 9 (size - 9)
        zipWithM_ run list $ fromIntegral <$> nats

transmitRS485 ::
    (KnownNat n, KnownNat l) =>
    List n RBUS ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
transmitRS485 list buff size = do
    when (size >? 2) do
        port <- deref $ buff ! 1
        let run RBUS{..} p = do
                shouldInit' <- deref shouldInit
                mode' <- deref mode
                when (iNot shouldInit' .&& mode' ==? modeRS485 .&& p ==? port) do
                    let size' = size - 2
                    RS.transmit rs \write ->
                        for (toIx size') \ix ->
                            write . safeCast =<< deref (buff ! (ix + 2))
                    store txLock true
        zipWithM_ run list $ fromIntegral <$> nats

initialize ::
    (KnownNat n, KnownNat l) =>
    List n RBUS ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
initialize list buff size =
    when (size ==? 33) do
        let run r@RBUS{..} offset = do
                store mode =<< unpack buff offset
                store baudrate =<< unpackBE buff (offset + 1)
                store lineControl =<< unpack buff (offset + 5)
                store sizeDMX512 =<< unpackBE buff (offset + 6)
                configureMode r
                store shouldInit false
        zipWithM_ run list $ fromIntegral <$> fromList [1, 9 ..]

configureMode :: RBUS -> Ivory eff ()
configureMode r = do
    mode' <- deref $ mode r
    cond_
        [ mode' ==? modeRBUS ==> configureRBUS r
        , mode' ==? modeRS485 ==> configureRS485 r
        , mode' ==? modeDMX512 ==> RS.configureRS485 (rs r) 250_000 I.WL_8b I.SB_1b I.None
        ]
    store (rxLock r) false
    I.clearRX $ rs r

configureRBUS :: RBUS -> Ivory eff ()
configureRBUS RBUS{..} = do
    I.configureRS485 rs defaultBaudrate I.WL_8b I.SB_1b I.None
    store shouldDiscovery false
    store shouldConfirm false
    store shouldPing 5
    Q.clear msgQueue
    reset protocol

configureRS485 :: RBUS -> Ivory eff ()
configureRS485 RBUS{..} = do
    baudrate' <- deref baudrate
    lineControl' <- deref lineControl
    let config lc wl sb p =
            lineControl'
                ==? lc
                ==> I.configureRS485 rs baudrate' wl sb p
    when (baudrate' >? 0) do
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

onDMX512 :: (KnownNat n, KnownNat l) => List n RBUS -> Buffer l Uint8 -> Uint8 -> Ivory eff ()
onDMX512 list buff size = do
    when (size >=? 4) do
        port <- deref $ buff ! 1
        let run RBUS{..} p = do
                shouldInit' <- deref shouldInit
                mode' <- deref mode
                sizeDMX <- deref sizeDMX512
                channel' <- unpackBE buff 2
                let channel = channel' - 1
                typeValue <- deref $ buff ! 4
                when
                    ( iNot shouldInit'
                        .&& mode'
                        ==? modeDMX512
                        .&& p
                        ==? port
                        .&& channel
                        <? sizeDMX
                    )
                    do
                        cond_
                            [ typeValue ==? 0 ==> do
                                ED.off dmx512 channel
                            , typeValue ==? 1 ==> do
                                ED.on dmx512 channel
                            , typeValue ==? 2 ==> do
                                brightness <- unpack buff 5 :: Ivory eff Uint8
                                ED.setBrightness dmx512 (safeCast brightness / 255) channel
                            , typeValue ==? 3 ==> do
                                brightness <- unpack buff 5 :: Ivory eff Uint8
                                velocity <- unpack buff 6 :: Ivory eff Uint8
                                ED.fade dmx512 (safeCast brightness / 255) (safeCast velocity / 255) channel
                            ]
        zipWithM_ run list $ fromIntegral <$> nats

dmx512Task :: RBUS -> Ivory (ProcEffects s t) ()
dmx512Task RBUS{..} = do
    mode' <- deref mode
    when (mode' ==? modeDMX512) do
        RS.configureRS485 rs 98_000 I.WL_8b I.SB_2b I.None
        RS.transmit rs \write -> do
            write 0 -- break byte 0
            write 0 -- first byte 0
            arrayMap (write <=< val)
        times (1_800 :: Ix 10_000) \_ -> nop 1
        RS.configureRS485 rs 250_000 I.WL_8b I.SB_2b I.None
  where
    val ix =
        castFloatToUint16 . (* 255)
            =<< deref ((ED.dmx512 dmx512 ! ix) ~> ED.value)

syncDMX512Task :: RBUS -> Ivory (ProcEffects s ()) ()
syncDMX512Task RBUS{..} = do
    shouldInit' <- deref shouldInit
    mode' <- deref mode
    when (mode' ==? modeDMX512 .&& iNot shouldInit') do
        i <- deref curSyncDMX512
        let dmx = ED.dmx512 dmx512 ! toIx i
        synced' <- deref $ dmx ~> ED.synced
        when (iNot synced') do
            msg <- ED.message dmx512 i
            T.transmitBuffer transport msg
            store (dmx ~> ED.synced) true
        sizeDMX512' <- deref sizeDMX512
        store curSyncDMX512 $ (i + 1) .% sizeDMX512'

calculateDMX512Task :: RBUS -> Ivory (ProcEffects s ()) ()
calculateDMX512Task RBUS{..} = do
    shouldInit' <- deref shouldInit
    mode' <- deref mode
    when (mode' ==? modeDMX512 .&& iNot shouldInit') do
        sizeDMX512' <- deref sizeDMX512
        for (toIx sizeDMX512') \ix -> do
            ED.calculateValue $ ED.dmx512 dmx512 ! ix

onGetState :: [RBUS] -> Ivory eff ()
onGetState = mapM_ run
  where
    run r@RBUS{..} = do
        shouldInit' <- deref shouldInit
        when (iNot shouldInit') do
            forceSyncRBUS r

{-
    TODO: handle actions only when initialized
-}
