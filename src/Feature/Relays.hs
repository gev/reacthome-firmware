{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use for_" #-}

module Feature.Relays where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import qualified Core.Domain as D
import Core.Task
import qualified Core.Transport as T
import Data.Buffer
import Data.Fixed
import Data.Foldable
import Data.Index
import Data.Record
import Data.Serialize
import Data.Value
import Endpoint.Groups (Groups (groups))
import qualified Endpoint.Groups as G
import Endpoint.Relays (delayOn)
import qualified Endpoint.Relays as R
import Feature.RS485.RBUS (initialize)
import GHC.TypeNats
import Interface.GPIO.Output
import Interface.GPIO.Port
import Interface.MCU (MCU, peripherals, systemClock)
import Interface.SystemClock (SystemClock, getSystemTime)
import Ivory.Language
import Ivory.Stdlib

data Relays n = forall o. (Output o) => Relays
    { n :: Int
    , getRelays :: R.Relays n
    , getGroups :: G.Groups n
    , getOutputs :: List n o
    , shouldInit :: Value IBool
    , clock :: SystemClock
    , current :: Index Uint8
    , transmit ::
        forall l.
        (KnownNat l) =>
        Buffer l Uint8 ->
        forall s t.
        Ivory (ProcEffects s t) ()
    }

relays ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , Output o
    , Pull p d
    , T.Transport t
    , KnownNat n
    ) =>
    List n (p -> d -> m o) ->
    t ->
    m (Relays n)
relays outs transport = do
    mcu <- asks D.mcu
    shouldInit <- asks D.shouldInit
    let peripherals' = peripherals mcu
    os <- mapM (($ pullNone peripherals') . ($ peripherals')) outs
    let n = length os
    getRelays <- R.mkRelays "relays"
    getGroups <- G.mkGroups "groups"
    current <- index "current_relay"
    let clock = systemClock mcu

    let relays =
            Relays
                { n
                , getRelays
                , getGroups
                , getOutputs = os
                , shouldInit
                , clock
                , current
                , transmit = T.transmitBuffer transport
                }

    addTask $ yeld "relays_manage" $ manageRelays relays
    addTask $ yeld "relays_sync" $ syncRelays relays

    addSync "relays" $ forceSyncRelays relays

    pure relays

forceSyncRelays :: (KnownNat n) => Relays n -> Ivory eff ()
forceSyncRelays Relays{..} = do
    arrayMap $ \ix -> store (R.relays getRelays ! ix ~> R.synced) false
    arrayMap $ \ix -> store (G.groups getGroups ! ix ~> G.synced) false

manageRelays :: (KnownNat n) => Relays n -> Ivory eff ()
manageRelays Relays{..} = do
    arrayMap $ \ix -> do
        let r = R.relays getRelays ! ix
        isOn <- deref $ r ~> R.state
        delayOn <- deref $ r ~> R.delayOn
        delayOff <- deref $ r ~> R.delayOff
        t0 <- deref $ r ~> R.timestamp
        t1 <- getSystemTime clock
        let dt = t1 - t0
        cond_
            [ iNot isOn
                .&& delayOn
                >? 0
                ==> when
                    (dt >? delayOn)
                    ( do
                        store (r ~> R.state) true
                        store (r ~> R.delayOn) 0
                        store (r ~> R.timestamp) t1
                    )
            , isOn
                .&& delayOff
                >? 0
                ==> when
                    (dt >? delayOff)
                    ( do
                        store (r ~> R.state) false
                        store (r ~> R.delayOn) 0
                        store (r ~> R.delayOff) 0
                        store (r ~> R.timestamp) t1
                    )
            ]

    zipWithM_ off getOutputs ints
    zipWithM_ on getOutputs ints
  where
    off :: (Output o) => o -> Int -> Ivory eff ()
    off output i = do
        let ix = fromIntegral i
        let r = R.relays getRelays ! ix
        isOff <- iNot <$> deref (r ~> R.state)
        when isOff $ manageState r output reset false

    on :: (Output o) => o -> Int -> Ivory eff ()
    on output i = do
        let ix = fromIntegral i
        let r = R.relays getRelays ! ix
        isOn <- deref $ r ~> R.state
        when isOn $ manageState r output set true

manageState ::
    (Output o) =>
    Record R.RelayStruct ->
    o ->
    (o -> Ivory eff ()) ->
    IBool ->
    Ivory eff ()
manageState r o setOut state = do
    state' <- get o
    when (state' /=? state) $ do
        store (r ~> R.synced) false
        setOut o

syncRelays ::
    (KnownNat n) =>
    Relays n ->
    Ivory (ProcEffects s ()) ()
syncRelays rs@Relays{..} = do
    shouldInit' <- deref shouldInit
    when (iNot shouldInit') $ do
        i <- deref current
        syncRelays' rs i
        syncGroups' rs i
        store current $ i + 1

syncRelays' ::
    (KnownNat n) =>
    Relays n ->
    Uint8 ->
    Ivory (ProcEffects s ()) ()
syncRelays' Relays{..} i = do
    let r = R.relays getRelays ! toIx i
    synced <- deref $ r ~> R.synced
    when (iNot synced) $ do
        msg <- R.message getRelays (i .% fromIntegral n)
        transmit msg
        store (r ~> R.synced) true

syncGroups' ::
    (KnownNat n) =>
    Relays n ->
    Uint8 ->
    Ivory (ProcEffects s ()) ()
syncGroups' Relays{..} i = do
    let g = G.groups getGroups ! toIx i
    synced <- deref $ g ~> G.synced
    when (iNot synced) $ do
        msg <- G.message getGroups (i .% fromIntegral n)
        transmit msg
        store (g ~> G.synced) true

onDo ::
    (KnownNat l, KnownNat n) =>
    Relays n ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onDo relays@Relays{..} buff size =
    when (size >=? 3) $ do
        index <- deref $ buff ! 1
        initialized <- iNot <$> deref shouldInit
        when (initialized .&& index >=? 1 .&& index <=? fromIntegral n) $ do
            action <- deref $ buff ! 2
            cond_
                [ action ==? 0 ==> R.turnOffRelay getRelays (toIx index)
                , action ==? 1 ==> do
                    cond_
                        [ size >=? 7 ==> R.turnOnRelay' getRelays getGroups (toIx index) =<< unpackLE buff 3
                        , true ==> R.turnOnRelay getRelays getGroups (toIx index)
                        ]
                , action ==? 2 .&& size >=? 7 ==> R.setRelayDelayOff getRelays index =<< unpackLE buff 3
                , action ==? 3 ==> R.setRelayGroup getRelays index =<< unpack buff 3
                ]

onGroup ::
    (KnownNat l, KnownNat n) =>
    Relays n ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory eff ()
onGroup Relays{..} buff size =
    when (size >=? 7) $ do
        index <- deref $ buff ! 1
        initialized <- iNot <$> deref shouldInit
        when (initialized .&& index >=? 1 .&& index <=? fromIntegral n) $ do
            let g = G.groups getGroups ! toIx (index - 1)
            store (g ~> G.enabled) =<< unpack buff 2
            store (g ~> G.delay) =<< unpackLE buff 3
            store (g ~> G.synced) false

{-
    TODO: Generalize initialization
-}
onInit ::
    (KnownNat l, KnownNat n) =>
    Relays n ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onInit rs@Relays{..} buff size =
    when (size >=? 1 + 5 * fromIntegral n + 6 * fromIntegral n) $ do
        offset <- local $ ival 1
        initGroups rs buff offset
        initRelays rs buff offset
        store shouldInit false

initGroups ::
    (KnownNat l, KnownNat n) =>
    Relays n ->
    Buffer l Uint8 ->
    Ref s (Stored (Ix l)) ->
    Ivory eff ()
initGroups Relays{..} buff offset =
    arrayMap $ \ix -> do
        offset' <- deref offset
        let g = G.groups getGroups ! ix
        store (g ~> G.enabled) =<< unpack buff offset'
        store (g ~> G.delay) =<< unpackLE buff (offset' + 1)
        store offset $ offset' + 5

initRelays ::
    (KnownNat l, KnownNat n) =>
    Relays n ->
    Buffer l Uint8 ->
    Ref s (Stored (Ix l)) ->
    Ivory eff ()
initRelays Relays{..} buff offset = do
    timestamp' <- getSystemTime clock
    arrayMap $ \ix -> do
        offset' <- deref offset
        let r = R.relays getRelays ! ix
        isLocked <- deref $ r ~> R.lock
        when (iNot isLocked) $ do
            store (r ~> R.state) =<< unpack buff offset'
            store (r ~> R.delayOff) =<< unpackLE buff (offset' + 2)
        store (r ~> R.group) =<< unpack buff (offset' + 1)
        store (r ~> R.defaultDelayOff) =<< unpackLE buff (offset' + 2)
        store (r ~> R.timestamp) timestamp'
        store offset $ offset' + 6

onGetState :: (KnownNat n) => Relays n -> Ivory eff ()
onGetState rs@Relays{..} = do
    initialized <- iNot <$> deref shouldInit
    when initialized $ forceSyncRelays rs
