module Implementation.MixV where

import Control.Monad.Reader hiding (local)
import Core.Actions
import Core.Controller
import Core.Domain qualified as D
import Data.Buffer
import Data.Serialize (unpack)
import Data.Value
import Endpoint.AOutputs qualified as EAO
import Endpoint.Dimmers qualified as EDIM
import Feature.AOutputs qualified as AO
import Feature.DInputs
import Feature.DS18B20
import Feature.Dimmers qualified as DIM
import Feature.Relays qualified as REL
import GHC.TypeLits
import Ivory.Language
import Ivory.Language.Proxy (NatType, aNat)
import Ivory.Stdlib

data Mix ni nd no nr = Mix
    { dinputs :: DInputs ni
    , aoutputs :: AO.AOutputs no
    , dimmers :: DIM.Dimmers nd
    , relays :: REL.Relays nr
    , shouldInit :: Value IBool
    }

mix ::
    forall no ni nd nr m p c t.
    (KnownNat no, Monad m, MonadReader (D.Domain p c) m) =>
    m t ->
    (Bool -> t -> m (DInputs ni)) ->
    (t -> m (AO.AOutputs no)) ->
    (t -> m (DIM.Dimmers nd)) ->
    (t -> m (REL.Relays nr)) ->
    (t -> m DS18B20) ->
    m (Mix ni nd no nr)
mix transport' dinputs' aoutputs' dimmers' relays' ds18b20 = do
    transport <- transport'
    dinputs <- dinputs' True transport
    aoutputs <- aoutputs' transport
    dimmers <- dimmers' transport
    relays <- relays' transport
    shouldInit <- asks D.shouldInit
    ds18b20 transport
    pure Mix{dinputs, dimmers, aoutputs, relays, shouldInit}

instance (KnownNat ni, KnownNat nd, KnownNat no, KnownNat nr) => Controller (Mix ni nd no nr) where
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
    forall l ni no nd nr s t.
    (KnownNat ni, KnownNat nd, KnownNat no, KnownNat nr, KnownNat l) =>
    Mix ni nd no nr ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onInit Mix{..} buff size = do
    let aoutputsN = fromIntegral $ natVal (aNat :: NatType no)
    let dimmersN = fromIntegral $ natVal (aNat :: NatType nd)
    let relaysN = fromIntegral $ natVal (aNat :: NatType nr)
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

onGetState ::
    (KnownNat ni, KnownNat nd, KnownNat no, KnownNat nr) =>
    Mix ni nd no nr ->
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
    Mix ni nd no nr ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onDim Mix{..} = do
    DIM.onDim dimmers

onAo ::
    (KnownNat l, KnownNat no) =>
    Mix ni nd no nr ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onAo Mix{..} = do
    AO.onAo aoutputs
