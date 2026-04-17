{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Implementation.MixF where

import Control.Monad.Reader hiding (local)
import Control.Monad.State
import Core.Actions
import Core.Context
import Core.Controller
import Core.Domain qualified as D
import Core.Task
import Core.Transport
import Data.Buffer
import Data.Serialize (Serialize (pack), unpack)
import Data.Type.Bool
import Data.Type.Equality
import Data.Value
import Endpoint.AOutputs qualified as EA
import Endpoint.AOutputs qualified as EAO
import Endpoint.DInputs qualified as DI
import Endpoint.Dimmers qualified as EDIM
import Endpoint.Dimmers qualified as EDim
import Feature.AOutputs qualified as AO
import Feature.DInputs
import Feature.DS18B20
import Feature.Dimmers qualified as DIM
import Feature.Dimmers qualified as FD
import Feature.GetInfo
import Feature.IndicatorFlush
import GHC.TypeLits
import Ivory.Language
import Ivory.Language.Proxy (NatType, aNat)
import Ivory.Stdlib
import Support.Cast

type ToSizeInBytes n = Div n 8 + If (Mod n 8 == 0) 0 1
type SizeSyncStateBuff ni nd na = 1 + ToSizeInBytes ni + nd + na

data Mix ni nd na = Mix
    { dinputs :: DInputs ni
    , dimmers :: DIM.Dimmers nd
    , aoutputs :: AO.AOutputs na
    , indicator :: IndicatorFlush 2
    , shouldInit :: Value IBool
    , syncStateBuff :: Buffer (SizeSyncStateBuff ni nd na) Uint8
    , info :: GetInfo
    , transmit ::
        forall n.
        (KnownNat n) =>
        Buffer n Uint8 ->
        forall s t.
        Ivory (ProcEffects s t) ()
    }

mix ::
    forall ni nd na m p c t.
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , KnownNat ni
    , KnownNat nd
    , KnownNat na
    , KnownNat (SizeSyncStateBuff ni nd na)
    , KnownNat (ToSizeInBytes ni)
    , Transport t
    , LazyTransport t
    ) =>
    (Bool -> t -> m (DInputs ni)) ->
    (t -> m (AO.AOutputs na)) ->
    (t -> m (DIM.Dimmers nd)) ->
    (t -> m DS18B20) ->
    (t -> m (IndicatorFlush 2)) ->
    m t ->
    m (Mix ni nd na)
mix dinputs' aoutputs' dimmers' ds18b20 indicator' transport' = do
    transport <- transport'
    dinputs <- dinputs' True transport
    aoutputs <- aoutputs' transport
    dimmers <- dimmers' transport
    indicator <- indicator' transport
    shouldInit <- asks D.shouldInit
    syncStateBuff <- buffer "mix_sync_channels"
    ds18b20 transport
    info <- mkGetMainInfo transport

    let mix =
            Mix
                { dinputs
                , dimmers
                , aoutputs
                , indicator
                , shouldInit
                , syncStateBuff
                , info
                , transmit = transmitBuffer transport
                }
    addTask $ delay 5_000 "sync_channels" $ syncChannels mix

    pure mix

instance
    ( KnownNat ni
    , KnownNat nd
    , KnownNat na
    , KnownNat (ToSizeInBytes ni)
    , KnownNat (SizeSyncStateBuff ni nd na)
    ) =>
    Controller (Mix ni nd na)
    where
    handle mix@Mix{..} buff size = do
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionDim ==> onDim mix buff size
            , action ==? actionAo ==> onAo mix buff size
            , action ==? actionInitialize ==> onInit mix buff size
            , action ==? actionGetState ==> onGetState mix
            , action ==? actionGetInfo ==> onGetInfo info
            , action ==? actionFindMe ==> onFindMe indicator buff size
            ]

onInit ::
    forall l ni nd na s t.
    (KnownNat ni, KnownNat nd, KnownNat na, KnownNat l) =>
    Mix ni nd na ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onInit Mix{..} buff size = do
    let aoutputsN = fromIntegral $ natVal (aNat :: NatType na)
    let dimmersN = fromIntegral $ natVal (aNat :: NatType nd)
    let sizeBuff = 1 + aoutputsN + (dimmersN * 3)
    when (size >=? sizeBuff) do
        offset <- local $ ival 1

        let aos = EAO.aoutputs $ AO.getAOutputs aoutputs
        arrayMap \ix -> do
            offset' <- deref offset
            let ao = aos ! ix
            v <- unpack buff offset' :: Ivory eff Uint8
            store (ao ~> EAO.value) (safeCast v / 255)
            store offset $ offset' + 1

        let ds = EDIM.dimmers $ DIM.getDimmers dimmers
        arrayMap \ix -> do
            let d = ds ! ix
            offset' <- deref offset
            group <- unpack buff offset'
            mode <- unpack buff (offset' + 1)
            value <- unpack buff (offset' + 2) :: Ivory eff Uint8
            EDIM.initialize d group mode (safeCast value / 255) 0
            EDIM.syncDimmerGroup ds d ix
            store offset $ offset' + 3

        store shouldInit false

        pure ()

syncChannels ::
    forall ni nd na s t.
    ( KnownNat ni
    , KnownNat nd
    , KnownNat na
    , KnownNat (SizeSyncStateBuff ni nd na)
    , KnownNat (ToSizeInBytes ni)
    ) =>
    Mix ni nd na ->
    Ivory (ProcEffects s t) ()
syncChannels Mix{..} = do
    shouldInit' <- deref shouldInit
    when (iNot shouldInit') do
        arrayMap \ix -> store (syncStateBuff ! ix) 0
        pack syncStateBuff 0 actionGetState

        offsetByte <- local $ ival 1
        offsetByte' <- deref offsetByte

        arrayMap \ix -> do
            let di' = DI.dinputs (getDInputs dinputs) ! ix
            diState <- deref $ di' ~> DI.state
            when diState do
                let ixByte = toIx $ offsetByte' + (fromIx ix `iDiv` 8)
                let numBit = castDefault $ fromIx ix .% 8
                let bitMask = 1 `iShiftL` numBit
                buffByte <- deref $ syncStateBuff ! ixByte
                pack syncStateBuff ixByte (buffByte .| bitMask)

        let numByteDI = fromIntegral $ natVal (aNat :: NatType (ToSizeInBytes ni))
        store offsetByte $ offsetByte' + numByteDI

        offsetByte'' <- deref offsetByte
        arrayMap \ix -> do
            let dimmer = EDim.dimmers (FD.getDimmers dimmers) ! ix
            dimmerBrightness <- castFloatToUint8 . (* 255) =<< deref (dimmer ~> EDim.brightness)
            let ixBuff = toIx . (+ offsetByte'') $ fromIx ix
            pack syncStateBuff ixBuff dimmerBrightness

        let numByteDIM = fromIntegral $ natVal (aNat :: NatType nd)
        store offsetByte $ offsetByte'' + numByteDIM

        offsetByte''' <- deref offsetByte
        arrayMap \ix -> do
            let aoutput = EA.aoutputs (AO.getAOutputs aoutputs) ! ix
            aoutputValue <- castFloatToUint8 . (* 255) =<< deref (aoutput ~> EA.value)
            let ixBuff = toIx . (+ offsetByte''') $ fromIx ix
            pack syncStateBuff ixBuff aoutputValue

        transmit syncStateBuff

onGetState ::
    (KnownNat ni, KnownNat nd, KnownNat na) =>
    Mix ni nd na ->
    Ivory eff ()
onGetState Mix{..} = do
    forceSyncDInputs dinputs
    initialized <- iNot <$> deref shouldInit
    when initialized do
        AO.forceSync aoutputs
        DIM.forceSync dimmers

onDim ::
    (KnownNat l, KnownNat nd) =>
    Mix ni nd na ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onDim Mix{..} = do
    DIM.onDim dimmers

onAo ::
    (KnownNat l, KnownNat na) =>
    Mix ni nd na ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onAo Mix{..} = do
    AO.onAo aoutputs
