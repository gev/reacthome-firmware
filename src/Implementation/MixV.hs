{-# LANGUAGE UndecidableInstances #-}

module Implementation.MixV where

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
import Endpoint.Relays qualified as R
import Feature.AOutputs qualified as AO
import Feature.DInputs
import Feature.DS18B20
import Feature.Dimmers qualified as DIM
import Feature.Dimmers qualified as FD
import Feature.Relays qualified as FR
import Feature.Relays qualified as REL
import GHC.TypeLits
import Ivory.Language
import Ivory.Language.Proxy (NatType, aNat)
import Ivory.Stdlib
import Support.Cast

type ToSizeInBytes n = Div n 8 + If (Mod n 8 == 0) 0 1
type SizeSyncStateBuff ni no nd na = 1 + ToSizeInBytes ni + ToSizeInBytes no + nd + na

data Mix ni no nd na = Mix
    { dinputs :: DInputs ni
    , dimmers :: DIM.Dimmers nd
    , relays :: REL.Relays no
    , aoutputs :: AO.AOutputs na
    , shouldInit :: Value IBool
    , syncStateBuff :: Buffer (SizeSyncStateBuff ni no nd na) Uint8
    , transmit ::
        forall n.
        (KnownNat n) =>
        Buffer n Uint8 ->
        forall s t.
        Ivory (ProcEffects s t) ()
    }

mix ::
    forall ni no nd na m p c t.
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , KnownNat ni
    , KnownNat no
    , KnownNat nd
    , KnownNat na
    , KnownNat (SizeSyncStateBuff ni no nd na)
    , KnownNat (ToSizeInBytes ni)
    , KnownNat (ToSizeInBytes no)
    , Transport t
    ) =>
    m t ->
    (Bool -> t -> m (DInputs ni)) ->
    (t -> m (AO.AOutputs na)) ->
    (t -> m (DIM.Dimmers nd)) ->
    (t -> m (REL.Relays no)) ->
    (t -> m DS18B20) ->
    m (Mix ni no nd na)
mix transport' dinputs' aoutputs' dimmers' relays' ds18b20 = do
    transport <- transport'
    dinputs <- dinputs' True transport
    aoutputs <- aoutputs' transport
    dimmers <- dimmers' transport
    relays <- relays' transport
    shouldInit <- asks D.shouldInit
    syncStateBuff <- buffer "mix_sync_channels"
    ds18b20 transport
    let mix =
            Mix
                { dinputs
                , dimmers
                , aoutputs
                , relays
                , shouldInit
                , syncStateBuff
                , transmit = transmitBuffer transport
                }
    addTask $ delay 5_000 "sync_channels" $ syncChannels mix

    pure mix

instance
    ( KnownNat ni
    , KnownNat nd
    , KnownNat no
    , KnownNat na
    , KnownNat (ToSizeInBytes ni)
    , KnownNat (ToSizeInBytes no)
    , KnownNat (SizeSyncStateBuff ni no nd na)
    ) =>
    Controller (Mix ni no nd na)
    where
    handle mix buff size = do
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionDo ==> onDo mix buff size
            , action ==? actionDim ==> onDim mix buff size
            , action ==? actionAo ==> onAo mix buff size
            , action ==? actionInitialize ==> onInit mix buff size
            , action ==? actionGetState ==> onGetState mix
            ]

onInit ::
    forall l ni no nd na s t.
    (KnownNat ni, KnownNat nd, KnownNat no, KnownNat na, KnownNat l) =>
    Mix ni no nd na ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onInit Mix{..} buff size = do
    let aoutputsN = fromIntegral $ natVal (aNat :: NatType na)
    let dimmersN = fromIntegral $ natVal (aNat :: NatType nd)
    let relaysN = fromIntegral $ natVal (aNat :: NatType no)
    let sizeBuff = 1 + aoutputsN + (dimmersN * 3) + (relaysN * 5 + relaysN * 6)
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

        REL.initGroups relays buff offset
        REL.initRelays relays buff offset

        store shouldInit false

        pure ()

syncChannels ::
    forall ni no nd na s t.
    ( KnownNat ni
    , KnownNat no
    , KnownNat nd
    , KnownNat na
    , KnownNat (SizeSyncStateBuff ni no nd na)
    , KnownNat (ToSizeInBytes ni)
    , KnownNat (ToSizeInBytes no)
    ) =>
    Mix ni no nd na ->
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
            let relay' = R.relays (FR.getRelays relays) ! ix
            relayState <- deref $ relay' ~> R.state
            when relayState do
                let ixByte = toIx $ offsetByte'' + (fromIx ix `iDiv` 8)
                let numBit = castDefault $ fromIx ix .% 8
                let bitMask = 1 `iShiftL` numBit
                buffByte <- deref $ syncStateBuff ! ixByte
                pack syncStateBuff ixByte (buffByte .| bitMask)

        let numByteDO = fromIntegral $ natVal (aNat :: NatType (ToSizeInBytes no))
        store offsetByte $ offsetByte'' + numByteDO

        offsetByte''' <- deref offsetByte
        arrayMap \ix -> do
            let dimmer = EDim.dimmers (FD.getDimmers dimmers) ! ix
            dimmerBrightness <- castFloatToUint8 . (* 255) =<< deref (dimmer ~> EDim.brightness)
            let ixBuff = toIx . (+ offsetByte''') $ fromIx ix
            pack syncStateBuff ixBuff dimmerBrightness

        let numByteDIM = fromIntegral $ natVal (aNat :: NatType nd)
        store offsetByte $ offsetByte''' + numByteDIM

        offsetByte'''' <- deref offsetByte
        arrayMap \ix -> do
            let aoutput = EA.aoutputs (AO.getAOutputs aoutputs) ! ix
            aoutputValue <- castFloatToUint8 . (* 255) =<< deref (aoutput ~> EA.value)
            let ixBuff = toIx . (+ offsetByte'''') $ fromIx ix
            pack syncStateBuff ixBuff aoutputValue

        transmit syncStateBuff

onGetState ::
    (KnownNat ni, KnownNat nd, KnownNat no, KnownNat na) =>
    Mix ni no nd na ->
    Ivory eff ()
onGetState Mix{..} = do
    forceSyncDInputs dinputs
    initialized <- iNot <$> deref shouldInit
    when initialized do
        AO.forceSync aoutputs
        DIM.forceSync dimmers
        REL.forceSyncRelays relays

onDo ::
    forall l ni no nd nr s t.
    (KnownNat l, KnownNat ni, KnownNat nd, KnownNat nr, KnownNat no) =>
    Mix ni nd no nr ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onDo Mix{..} = do
    REL.onDo relays

onDim ::
    (KnownNat l, KnownNat nd) =>
    Mix ni no nd na ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onDim Mix{..} = do
    DIM.onDim dimmers

onAo ::
    (KnownNat l, KnownNat na) =>
    Mix ni no nd na ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onAo Mix{..} = do
    AO.onAo aoutputs
