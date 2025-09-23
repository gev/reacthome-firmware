{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Implementation.Mix where

import Control.Monad (zipWithM_)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import Core.Controller
import Core.Domain as D
import Core.Task
import Core.Transport
import qualified Core.Transport as T
import Data.Buffer
import Data.Matrix
import Data.Serialize
import Data.Value
import Endpoint.ATS as A
import qualified Endpoint.DInputs as DI
import Endpoint.DInputsRelaysRules as Ru
import qualified Endpoint.Groups as G
import qualified Endpoint.Relays as R
import Feature.DInputs as FDI (
    DInputs (DInputs, getDInputs, getInputs),
    dinputs,
    forceSyncDInputs,
    manageDInputs,
    syncDInputs,
 )
import Feature.Mix.Indicator as FI (
    Indicator,
    indicator,
    onFindMe,
 )
import Feature.Relays as FR (
    Relays (Relays, getGroups, getRelays),
    forceSyncRelays,
    manageRelays,
    n,
    onDo,
    onGroup,
    onInit,
    relays,
    syncRelays,
 )
import GHC.RTS.Flags (DebugFlags (stable))
import GHC.TypeNats
import Interface.Display
import Interface.Flash as F
import Interface.GPIO.Input
import Interface.GPIO.Output
import Interface.GPIO.Port
import Interface.MCU as I
import Ivory.Language
import Ivory.Language.Proxy
import Ivory.Stdlib
import Util.CRC16
import Prelude hiding (error)

data Mix ni no = forall f. (Flash f) => Mix
    { relays :: Relays no
    , dinputs :: DInputs ni
    , rules :: Rules ni no
    , ats :: ATS
    , indicator :: Indicator ni no
    , etc :: f
    , shouldInit :: Value IBool
    , shouldSaveConfig :: Value IBool
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
    , KnownNat (PayloadSize no)
    ) =>
    m t ->
    (Bool -> t -> m (DInputs ni)) ->
    (t -> m (Relays no)) ->
    ( ATS ->
      DI.DInputs ni ->
      R.Relays no ->
      t ->
      m (Indicator ni no)
    ) ->
    (p -> f) ->
    m (Mix ni no)
mix transport' dinputs' relays' indicator' etc = do
    transport <- transport'
    relays <- relays' transport
    dinputs <- dinputs' True transport
    rules <- mkRules transport
    ats <- mkATS transport
    indicator <- indicator' ats (getDInputs dinputs) (getRelays relays) transport
    mcu <- asks D.mcu
    shouldInit <- asks D.shouldInit
    shouldSaveConfig <- value "mix_should_save_config" false
    saveCountdown <- value "mix_save_save_countdown" 0
    let mix =
            Mix
                { relays
                , dinputs
                , rules
                , ats
                , indicator
                , etc = etc (peripherals mcu)
                , shouldInit
                , shouldSaveConfig
                , saveCountdown
                , transmit = T.transmitBuffer transport
                }

    addInit "mix" $ load mix

    addTask $ delay 10 "mix_manage" $ manage mix
    addTask $ delay 1 "mix_sync" $ sync mix
    addTask $ delay 1 "mix_save_config" $ saveTask mix

    addSync "dinputs" $ forceSyncDInputs dinputs
    addSync "relays" $ forceSyncRelays relays
    addSync "rules" $ forceSyncRules rules
    addSync "ats" $ forceSyncATS ats

    pure mix

manage ::
    (KnownNat ni, KnownNat no) =>
    Mix ni no ->
    Ivory ('Effects (Returns ()) r (Scope s)) ()
manage Mix{..} = do
    manageDInputs dinputs
    manageRules rules (getDInputs dinputs) (getRelays relays) (getGroups relays)
    manageATS ats (getDInputs dinputs) (getRelays relays)
    manageRelays relays

sync ::
    (KnownNat ni, KnownNat no, KnownNat (PayloadSize no)) =>
    Mix ni no ->
    Ivory (ProcEffects s ()) ()
sync Mix{..} = do
    syncDInputs dinputs
    syncRelays relays
    syncRules rules
    syncATS ats

instance (KnownNat ni, KnownNat no, KnownNat (PayloadSize no)) => Controller (Mix ni no) where
    handle mix@Mix{..} buff size = do
        shouldInit' <- deref shouldInit
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionDo .&& iNot shouldInit' ==> onDo relays buff size
            , action ==? actionGroup .&& iNot shouldInit' ==> onGroup relays buff size
            , action ==? actionDiRelaySync .&& iNot shouldInit' ==> onRule mix buff size
            , action ==? actionMix .&& iNot shouldInit' ==> onMode mix buff size
            , action ==? actionInitialize ==> onInit relays buff size
            , action ==? actionGetState ==> onGetState mix
            , action ==? actionFindMe ==> onFindMe indicator buff size
            , action ==? actionError ==> resetError ats
            ]

onRule ::
    forall l ni no s t.
    (KnownNat l, KnownNat ni, KnownNat no, KnownNat (PayloadSize no)) =>
    Mix ni no ->
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

onMode ::
    (KnownNat l, KnownNat ni, KnownNat no) =>
    Mix ni no ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onMode mix@Mix{..} buff size = do
    when (size ==? 2) $ do
        store (mode ats) =<< unpack buff 1
        manageLock mix
        resetError ats
        store shouldSaveConfig true

onGetState :: (KnownNat ni, KnownNat no) => Mix ni no -> Ivory eff ()
onGetState Mix{..} = do
    forceSyncDInputs dinputs
    forceSyncRules rules
    forceSyncATS ats
    initialized <- iNot <$> deref shouldInit
    when initialized $ do
        forceSyncRelays relays

saveTask :: (KnownNat ni, KnownNat no) => Mix ni no -> Ivory (ProcEffects s t) ()
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

save :: (KnownNat ni, KnownNat no) => Mix ni no -> Ivory (ProcEffects s t) ()
save Mix{..} = do
    erasePage etc 0
    crc <- local $ istruct initCRC16
    mode' <- deref (mode ats)
    updateCRC16 crc mode'
    F.write etc 0 $ safeCast mode'
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

load :: (KnownNat ni, KnownNat no) => Mix ni no -> Ivory (ProcEffects s ()) ()
load mix@Mix{..} = do
    valid <- checkCRC mix
    when valid $ do
        store (mode ats) . castDefault =<< F.read etc 0
        manageLock mix
        kx <- local $ ival 4
        let run rules = arrayMap $ \ix -> arrayMap $ \jx -> do
                kx' <- deref kx
                store (rules ! ix ! jx) . castDefault =<< F.read etc kx'
                store kx $ kx' + 4
        run $ rulesOff rules
        run $ rulesOn rules

checkCRC :: forall ni no s. (KnownNat ni, KnownNat no) => Mix ni no -> Ivory (ProcEffects s ()) IBool
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

manageLock Mix{..} = do
    let r' = R.relays $ getRelays relays
    mode' <- deref $ mode ats
    arrayMap $ \ix -> store (r' ! ix ~> R.lock) false
    cond_
        [ mode' ==? mode_N1_G ==> do
            store (r' ! 0 ~> R.lock) true
            store (r' ! 4 ~> R.lock) true
            store (r' ! 5 ~> R.lock) true
        , mode' ==? mode_N2 ==> do
            store (r' ! 0 ~> R.lock) true
            store (r' ! 1 ~> R.lock) true
        , mode' ==? mode_N2_G ==> do
            store (r' ! 0 ~> R.lock) true
            store (r' ! 1 ~> R.lock) true
            store (r' ! 4 ~> R.lock) true
            store (r' ! 5 ~> R.lock) true
        ]
