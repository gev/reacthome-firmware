module Implementation.DIRSM where

import Control.Monad.Reader (MonadReader, asks)
import Core.Actions
import Core.Controller
import Core.Domain qualified as D
import Data.Buffer
import Data.Fixed (List, fromList, zipWithM_)
import Data.Serialize
import Data.Value
import Endpoint.AOutputs qualified as A
import Feature.AOutputs (
    AOutputs (getAOutputs),
    forceSync,
    n,
    onAo,
 )
import Feature.DInputs (DInputs, forceSyncDInputs)
import Feature.DS18B20
import Feature.RS485.RSM (
    configureMode,
    forceSyncRSM',
    setMode,
    transmitRS485,
 )
import Feature.RS485.RSM.Data
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib

data DIRSM ni no nr = DIRSM
    { rsm :: List nr RSM
    , dinputs :: DInputs ni
    , aoutputs :: AOutputs no
    , shouldInit :: Value IBool
    }

diRsm ::
    (MonadReader (D.Domain p c) m) =>
    m t ->
    (Bool -> t -> m (DInputs ni)) ->
    (t -> m (List nr RSM)) ->
    (t -> m (AOutputs no)) ->
    (t -> m DS18B20) ->
    m (DIRSM ni no nr)
diRsm transport' dinputs' rsm' aoutputs' ds18b20 = do
    shouldInit <- asks D.shouldInit
    transport <- transport'
    ds18b20 transport
    rsm <- rsm' transport
    dinputs <- dinputs' True transport
    aoutputs <- aoutputs' transport
    pure DIRSM{rsm, dinputs, aoutputs, shouldInit}

instance (KnownNat ni, KnownNat no, KnownNat nr) => Controller (DIRSM ni no nr) where
    handle s@DIRSM{..} buff size = do
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionAo ==> onAo aoutputs buff size
            , action ==? actionInitialize ==> onInit s buff size
            , action ==? actionRs485Mode ==> setMode rsm buff size
            , action ==? actionRs485Transmit ==> transmitRS485 rsm buff size
            , action ==? actionGetState ==> onGetState s
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
