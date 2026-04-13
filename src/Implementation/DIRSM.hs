{-# LANGUAGE UndecidableInstances #-}

module Implementation.DIRSM where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State
import Core.Actions
import Core.Context
import Core.Controller
import Core.Domain qualified as D
import Core.Task
import Core.Transport
import Data.Buffer
import Data.Fixed (List, fromList, zipWithM_)
import Data.Serialize
import Data.Type.Bool
import Data.Type.Equality
import Data.Value
import Endpoint.AOutputs qualified as A
import Endpoint.DInputs qualified as D
import Feature.AOutputs (
    AOutputs (getAOutputs),
    forceSync,
    n,
    onAo,
 )
import Feature.DInputs (DInputs (getDInputs, transmit), forceSyncDInputs)
import Feature.DS18B20
import Feature.GetInfo
import Feature.RS485.RSM (
    configureMode,
    forceSyncRSM',
    setMode,
    transmitRS485,
 )
import Feature.RS485.RSM.Data
import GHC.TypeNats
import Ivory.Language
import Ivory.Language.Proxy
import Ivory.Stdlib
import Support.Cast

type ToSizeInBytes n = Div n 8 + If (Mod n 8 == 0) 0 1
type SizeSyncStateBuff ni no = 1 + ToSizeInBytes ni + no

data DIRSM ni no nr = DIRSM
    { rsm :: List nr RSM
    , dinputs :: DInputs ni
    , aoutputs :: AOutputs no
    , shouldInit :: Value IBool
    , syncStateBuff :: Buffer (SizeSyncStateBuff ni no) Uint8
    , info :: GetInfo
    }

diRsm ::
    ( MonadReader (D.Domain p c) m
    , KnownNat (SizeSyncStateBuff ni no)
    , MonadState Context m
    , KnownNat ni
    , KnownNat no
    , KnownNat (ToSizeInBytes ni)
    , LazyTransport t
    ) =>
    (Bool -> t -> m (DInputs ni)) ->
    (t -> m (List nr RSM)) ->
    (t -> m (AOutputs no)) ->
    (t -> m DS18B20) ->
    m t ->
    m (DIRSM ni no nr)
diRsm dinputs' rsm' aoutputs' ds18b20 transport' = do
    shouldInit <- asks D.shouldInit
    transport <- transport'
    ds18b20 transport
    rsm <- rsm' transport
    dinputs <- dinputs' True transport
    aoutputs <- aoutputs' transport
    syncStateBuff <- buffer "sync_channels"
    info <- mkGetMainInfo transport

    let dirsm =
            DIRSM
                { rsm
                , dinputs
                , aoutputs
                , shouldInit
                , syncStateBuff
                , info
                }

    addTask $ delay 5_000 "sync_channels" $ syncChannels dirsm

    pure dirsm

instance
    ( KnownNat ni
    , KnownNat no
    , KnownNat nr
    , KnownNat (SizeSyncStateBuff ni no)
    , KnownNat (ToSizeInBytes ni)
    ) =>
    Controller (DIRSM ni no nr)
    where
    handle s@DIRSM{..} buff size = do
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionAo ==> onAo aoutputs buff size
            , action ==? actionInitialize ==> onInit s buff size
            , action ==? actionRs485Mode ==> setMode rsm buff size
            , action ==? actionRs485Transmit ==> transmitRS485 rsm buff size
            , action ==? actionGetState ==> onGetState s
            , action ==? actionGetInfo ==> onGetInfo info
            ]

onInit ::
    (KnownNat l, KnownNat ni, KnownNat no, KnownNat nr) =>
    DIRSM ni no nr ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onInit DIRSM{..} buff size = do
    let s = 1 + (5 * fromIntegral (length rsm))
    when (size ==? s + n aoutputs) do
        let run r@RSM{..} offset = do
                store baudrate =<< unpackLE buff offset
                store lineControl =<< unpack buff (offset + 4)
                configureMode r
        zipWithM_ run rsm $ fromIntegral <$> fromList [1, 6 ..]

        offset <- local $ ival $ toIx s
        let aos = A.aoutputs $ getAOutputs aoutputs
        arrayMap \ix -> do
            offset' <- deref offset
            let ao = aos ! ix
            value <- deref $ buff ! offset'
            store (ao ~> A.value) $ safeCast value / 255
            store (ao ~> A.synced) false
            store offset $ offset' + 1

        store shouldInit false

onGetState DIRSM{..} = do
    forceSyncDInputs dinputs
    forceSync aoutputs
    forceSyncRSM' rsm

syncChannels ::
    forall ni no nr s.
    ( KnownNat ni
    , KnownNat no
    , KnownNat (SizeSyncStateBuff ni no)
    , KnownNat (ToSizeInBytes ni)
    ) =>
    DIRSM ni no nr ->
    Ivory (ProcEffects s ()) ()
syncChannels DIRSM{..} = do
    shouldInit' <- deref shouldInit
    when (iNot shouldInit') do
        arrayMap \ix -> store (syncStateBuff ! ix) 0
        pack syncStateBuff 0 actionGetState

        let startOffset = 1
        offsetByte <- local $ ival startOffset
        offsetByte' <- deref offsetByte

        arrayMap \ix -> do
            let input = D.dinputs (getDInputs dinputs) ! ix
            diState <- deref $ input ~> D.state
            when diState do
                let ixByte = toIx $ offsetByte' + (fromIx ix `iDiv` 8)
                let numBit = castDefault $ fromIx ix .% 8
                byteFromBuff <- deref $ syncStateBuff ! ixByte
                let newByte = byteFromBuff .| (1 `iShiftL` numBit)
                pack syncStateBuff ixByte newByte

        let numByteDI = fromIntegral $ natVal (aNat :: NatType (ToSizeInBytes ni))
        store offsetByte $ offsetByte' + numByteDI

        arrayMap \ix -> do
            offset <- deref offsetByte
            let aoutput = A.aoutputs (getAOutputs aoutputs) ! ix
            aoutputValue <- castFloatToUint8 . (* 255) =<< deref (aoutput ~> A.value)
            let ixBuff = toIx . (+ offset) $ fromIx ix
            pack syncStateBuff ixBuff aoutputValue

        transmit dinputs syncStateBuff