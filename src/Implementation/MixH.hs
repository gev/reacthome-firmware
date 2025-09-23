{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Implementation.MixH where

import Control.Monad (zipWithM_)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import Core.Controller
import Core.Domain as D
import Core.Task
import Core.Transport
import Data.Buffer
import Data.Serialize
import Data.Value
import Endpoint.DInputsRelaysRules as Ru
import Endpoint.Dimmers as EDim (
    dimmers,
    initialize,
    syncDimmerGroup,
 )
import Feature.DInputs as FDI (
    DInputs (DInputs, getDInputs, getInputs),
    dinputs,
    forceSyncDInputs,
    manageDInputs,
    syncDInputs,
 )
import Feature.DS18B20 (DS18B20, ds18b20)
import Feature.Dimmers as FDim (Dimmers, forceSync, getDimmers, n, onDim)
import Feature.Relays as FR (
    Relays (Relays, getGroups, getRelays),
    forceSyncRelays,
    initGroups,
    initRelays,
    manageRelays,
    n,
    onDo,
    onGroup,
    relays,
    syncRelays,
 )
import GHC.TypeNats
import Interface.Flash as F
import Interface.MCU as I
import Ivory.Language
import Ivory.Language.Proxy
import Ivory.Stdlib
import Util.CRC16

data Mix ni no nd = forall f. (Flash f) => Mix
    { relays :: Relays no
    , dinputs :: DInputs ni
    , dimmers :: Dimmers nd
    , rules :: Rules ni no
    , etc :: f
    , shouldSaveConfig :: Value IBool
    , shouldInit :: Value IBool
    , saveCountdown :: Value Uint8
    , transmit ::
        forall n.
        (KnownNat n) =>
        Buffer n Uint8 ->
        forall s t.
        Ivory (ProcEffects s t) ()
    }

mix ::
    ( MonadState Context m
    , MonadReader (Domain p c) m
    , Flash f
    , Transport t
    , KnownNat ni
    , KnownNat no
    , KnownNat nd
    , KnownNat (PayloadSize no)
    ) =>
    m t ->
    (Bool -> t -> m (DInputs ni)) ->
    (t -> m (Relays no)) ->
    (t -> m (Dimmers nd)) ->
    (t -> m DS18B20) ->
    (p -> f) ->
    m (Mix ni no nd)
mix transport' dinputs' relays' dimmers' ds18b20 etc = do
    transport <- transport'
    relays <- relays' transport
    dinputs <- dinputs' True transport
    dimmers <- dimmers' transport
    rules <- mkRules transport
    mcu <- asks D.mcu
    shouldInit <- asks D.shouldInit
    shouldSaveConfig <- value "mix_should_save_config" false
    saveCountdown <- value "mix_save_save_countdown" 0
    ds18b20 transport

    let mix =
            Mix
                { relays
                , dinputs
                , dimmers
                , rules
                , etc = etc (peripherals mcu)
                , shouldSaveConfig
                , shouldInit
                , saveCountdown
                , transmit = transmitBuffer transport
                }

    addInit "mix" $ load mix

    addTask $ delay 10 "mix_manage" $ manage mix
    addTask $ delay 1 "mix_sync" $ sync mix
    addTask $ delay 1 "mix_save_config" $ saveTask mix

    addSync "dinputs" $ forceSyncDInputs dinputs
    addSync "relays" $ forceSyncRelays relays
    addSync "dimmers" $ FDim.forceSync dimmers
    addSync "rules" $ forceSyncRules rules

    pure mix

manage ::
    (KnownNat ni, KnownNat no) =>
    Mix ni no nd ->
    Ivory ('Effects (Returns ()) r (Scope s)) ()
manage Mix{..} = do
    manageDInputs dinputs
    manageRules rules (getDInputs dinputs) (getRelays relays) (getGroups relays)
    manageRelays relays

sync ::
    (KnownNat ni, KnownNat no, KnownNat nd, KnownNat (PayloadSize no)) =>
    Mix ni no nd ->
    Ivory (ProcEffects s ()) ()
sync Mix{..} = do
    syncDInputs dinputs
    syncRelays relays
    syncRules rules

instance (KnownNat ni, KnownNat no, KnownNat nd, KnownNat (PayloadSize no)) => Controller (Mix ni no nd) where
    handle mix@Mix{..} buff size = do
        shouldInit' <- deref shouldInit
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionDo .&& iNot shouldInit' ==> onDo relays buff size
            , action ==? actionGroup .&& iNot shouldInit' ==> onGroup relays buff size
            , action ==? actionDim .&& iNot shouldInit' ==> onDim dimmers buff size
            , action ==? actionDiRelaySync .&& iNot shouldInit' ==> onRule mix buff size
            , action ==? actionInitialize ==> onInit mix buff size
            , action ==? actionGetState ==> onGetState mix
            ]

onRule ::
    forall l ni no nd s t.
    (KnownNat l, KnownNat ni, KnownNat no, KnownNat nd, KnownNat (PayloadSize no)) =>
    Mix ni no nd ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onRule mix@Mix{..} buff size = do
    let relaysN = fromIntegral $ natVal (aNat :: NatType no)
    let dinputsN = fromIntegral $ natVal (aNat :: NatType ni)
    i <- subtract 1 <$> deref (buff ! 1)
    when (size ==? 2 + 2 * relaysN .&& i <? dinputsN) $ do
        kx <- local $ ival 2
        let run rules = arrayMap $ \jx -> do
                kx' <- deref kx
                store (rules ! toIx i ! jx) =<< unpack buff kx'
                store kx $ kx' + 1
        run $ rulesOff rules
        run $ rulesOn rules
        fillPayload rules i
        transmit $ Ru.payload rules
        store shouldSaveConfig true

onGetState :: (KnownNat ni, KnownNat no, KnownNat nd) => Mix ni no nd -> Ivory eff ()
onGetState Mix{..} = do
    forceSyncDInputs dinputs
    forceSyncRules rules
    initialized <- iNot <$> deref shouldInit
    when initialized $ do
        forceSyncRelays relays
        FDim.forceSync dimmers

saveTask :: (KnownNat ni, KnownNat no) => Mix ni no nd -> Ivory (ProcEffects s t) ()
saveTask mix@Mix{..} = do
    shouldSaveConfig' <- deref shouldSaveConfig
    when shouldSaveConfig' $ do
        store saveCountdown 100
        store shouldSaveConfig false
    saveCountdown' <- deref saveCountdown
    when (saveCountdown' >? 0) $ do
        store saveCountdown (saveCountdown' - 1)
    when (saveCountdown' ==? 1) $
        save mix

save :: (KnownNat ni, KnownNat no) => Mix ni no nd -> Ivory (ProcEffects s t) ()
save Mix{..} = do
    erasePage etc 0
    crc <- local $ istruct initCRC16
    kx <- local $ ival 4
    let run rules = arrayMap $ \ix -> arrayMap $ \jx -> do
            kx' <- deref kx
            v <- deref (rules ! ix ! jx)
            updateCRC16 crc v
            F.write etc kx' $ safeCast v
            store kx $ kx' + 4
    run $ rulesOff rules
    run $ rulesOn rules
    kx' <- deref kx
    F.write etc kx' . safeCast =<< deref (crc ~> msb)
    F.write etc (kx' + 4) . safeCast =<< deref (crc ~> lsb)

load :: (KnownNat ni, KnownNat no) => Mix ni no nd -> Ivory (ProcEffects s ()) ()
load mix@Mix{..} = do
    valid <- checkCRC mix
    when valid $ do
        kx <- local $ ival 4
        let run rules = arrayMap $ \ix -> arrayMap $ \jx -> do
                kx' <- deref kx
                store (rules ! ix ! jx) . castDefault =<< F.read etc kx'
                store kx $ kx' + 4
        run $ rulesOff rules
        run $ rulesOn rules

checkCRC :: forall ni no nd s. (KnownNat ni, KnownNat no) => Mix ni no nd -> Ivory (ProcEffects s ()) IBool
checkCRC Mix{..} = do
    let relaysN = fromIntegral $ natVal (aNat :: NatType no)
    let dinputsN = fromIntegral $ natVal (aNat :: NatType ni)
    crc <- local $ istruct initCRC16
    updateCRC16 crc . castDefault =<< F.read etc 0
    kx <- local $ ival 4
    times (2 * dinputsN * relaysN :: Ix 256) $ \_ -> do
        kx' <- deref kx
        updateCRC16 crc . castDefault =<< F.read etc kx'
        store kx $ kx' + 4
    kx' <- deref kx
    msb' <- castDefault <$> F.read etc kx'
    lsb' <- castDefault <$> F.read etc (kx' + 4)
    lsb'' <- deref $ crc ~> lsb
    msb'' <- deref $ crc ~> msb
    pure $ lsb' ==? lsb'' .&& msb' ==? msb''

onInit :: (KnownNat no, KnownNat nd, ANat l) => Mix ni no nd -> Buffer l Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onInit m@Mix{..} buff size = do
    let relsSizeBuff = 1 + 5 * fromIntegral (FR.n relays) + 6 * fromIntegral (FR.n relays)
    let dimsSizeBuff = FDim.n dimmers * 3

    when (size >=? relsSizeBuff + dimsSizeBuff) $ do
        offset <- local $ ival 1

        initGroups relays buff offset
        initRelays relays buff offset

        let ds = EDim.dimmers (FDim.getDimmers dimmers)
        arrayMap $ \ix -> do
            offset' <- deref offset
            let d = ds ! ix
            group <- unpack buff offset'
            mode <- unpack buff (offset' + 1)
            value <- unpack buff (offset' + 2) :: Ivory eff Uint8
            EDim.initialize d group mode (safeCast value / 255) 0
            EDim.syncDimmerGroup ds d ix
            store offset $ offset' + 3

        store shouldInit false
