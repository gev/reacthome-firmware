{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Endpoint.Relays where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import qualified Core.Domain as D
import Data.Buffer
import Data.Record
import Data.Serialize
import Endpoint.Groups (Groups (groups))
import qualified Endpoint.Groups as G
import GHC.TypeNats
import Interface.MCU
import Interface.SystemClock (SystemClock, getSystemTime)
import Ivory.Language
import Ivory.Language.Proxy
import Ivory.Stdlib

type RelayStruct = "relay_struct"

[ivory|
    struct relay_struct
    { state :: IBool
    ; defaultDelayOff :: Uint32
    ; delayOff :: Uint32
    ; delayOn :: Uint32
    ; timestamp :: Uint32
    ; group :: Uint8
    ; synced :: IBool
    ; lock :: IBool
    }
|]

data Relays n = Relays
    { relays :: Records n RelayStruct
    , payload :: Buffer 8 Uint8
    , clock :: SystemClock
    }

mkRelays ::
    forall n p i m.
    ( MonadState Context m
    , MonadReader (D.Domain p i) m
    , KnownNat n
    ) =>
    String ->
    m (Relays n)
mkRelays name = do
    addStruct (Proxy :: Proxy RelayStruct)
    mcu <- asks D.mcu
    let clock = systemClock mcu
    relays <-
        records name $
            go . fromIntegral
                <$> [1 .. natVal (aNat :: NatType n)]
    payload <- buffer "relay_message"
    pure Relays{relays, payload, clock}
  where
    go i =
        [ state .= ival false
        , defaultDelayOff .= ival 0
        , delayOn .= ival 0
        , delayOff .= ival 0
        , timestamp .= ival 0
        , group .= ival i
        , synced .= ival false
        , lock .= ival false
        ]

message ::
    (KnownNat n) =>
    Relays n ->
    Uint8 ->
    Ivory eff (Buffer 8 Uint8)
message Relays{..} i = do
    let relay = relays ! toIx i
    pack payload 0 actionDo
    pack payload 1 $ i + 1
    pack payload 2 =<< deref (relay ~> state)
    pack payload 3 =<< deref (relay ~> group)
    packLE payload 4 =<< deref (relay ~> defaultDelayOff)
    pure payload

turnOffRelay ::
    (KnownNat n) =>
    Relays n ->
    Ix n ->
    Ivory eff ()
turnOffRelay Relays{..} index = do
    let ix = index - 1
    let r = relays ! ix
    isLocked <- deref $ r ~> lock
    shouldOff <- deref $ r ~> state
    when (iNot isLocked .&& shouldOff) $ do
        store (r ~> state) false
        store (r ~> delayOn) 0
        store (r ~> delayOff) 0
        store (r ~> timestamp) =<< getSystemTime clock

turnOnRelay ::
    (KnownNat n) =>
    Relays n ->
    G.Groups n ->
    Ix n ->
    Ivory ('Effects (Returns t) b (Scope s)) ()
turnOnRelay Relays{..} groups index = do
    let ix = index - 1
    let r = relays ! ix
    isLocked <- deref $ r ~> lock
    delayOn' <- deref $ r ~> delayOn
    isOn <- deref $ r ~> state
    when (iNot isLocked .&& iNot isOn .&& delayOn' ==? 0) $ do
        timestamp' <- getSystemTime clock
        group' <- deref $ r ~> group
        turnOffGroup relays ix group' timestamp'
        delay' <- getGroupDelay relays groups group' timestamp'
        store (r ~> delayOn) delay'
        store (r ~> timestamp) timestamp'
        when (delay' ==? 0) $ store (r ~> state) true
    store (r ~> delayOff) =<< deref (r ~> defaultDelayOff)

turnOnRelay' ::
    (KnownNat n) =>
    Relays n ->
    G.Groups n ->
    Ix n ->
    Uint32 ->
    Ivory ('Effects (Returns t) b (Scope s)) ()
turnOnRelay' Relays{..} groups index delayOff' = do
    let ix = index - 1
    let r = relays ! ix
    isLocked <- deref $ r ~> lock
    delayOn' <- deref $ r ~> delayOn
    isOn <- deref $ r ~> state
    when (iNot isLocked .&& iNot isOn .&& delayOn' ==? 0) $ do
        timestamp' <- getSystemTime clock
        group' <- deref $ r ~> group
        turnOffGroup relays ix group' timestamp'
        delay' <- getGroupDelay relays groups group' timestamp'
        store (r ~> delayOn) delay'
        store (r ~> timestamp) timestamp'
        when (delay' ==? 0) $ store (r ~> state) true
    store (r ~> delayOff) delayOff'

toggleRelay ::
    (KnownNat n) =>
    Relays n ->
    G.Groups n ->
    Ix n ->
    Ivory ('Effects (Returns t) b (Scope s)) ()
toggleRelay rs@Relays{..} groups index = do
    let ix = index - 1
    let r = relays ! ix
    state' <- deref $ r ~> state
    ifte_
        state'
        (turnOffRelay rs index)
        (turnOnRelay rs groups index)

setRelayDelayOff :: (KnownNat n) => Relays n -> Uint8 -> Uint32 -> Ivory eff ()
setRelayDelayOff Relays{..} index delay = do
    let ix = toIx (index - 1)
    let r = relays ! ix
    store (r ~> defaultDelayOff) delay
    store (r ~> synced) false

setRelayGroup ::
    (KnownNat n) =>
    Relays n ->
    Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
setRelayGroup Relays{..} index group' = do
    let ix = toIx (index - 1)
    let r = relays ! ix
    turnOffGroup relays ix group' =<< getSystemTime clock
    store (r ~> group) group'
    store (r ~> synced) false

getGroupDelay ::
    (KnownNat n) =>
    Records n RelayStruct ->
    G.Groups n ->
    Uint8 ->
    Uint32 ->
    Ivory ('Effects (Returns t) b (Scope s)) Uint32
getGroupDelay rs G.Groups{..} i ts = do
    delay' <- deref $ groups ! toIx (i - 1) ~> G.delay
    min <- local $ ival delay'
    arrayMap $ \jx -> do
        let r = rs ! jx
        isLocked <- deref $ r ~> lock
        when (iNot isLocked) $ do
            isOn <- deref $ r ~> state
            group' <- deref $ r ~> group
            when (iNot isOn .&& group' ==? i) $ do
                dt <- (ts -) <$> deref (r ~> timestamp)
                min' <- deref min
                when (dt <? min') $ store min dt
    min' <- deref min
    ifte
        (delay' >? min')
        (pure $ delay' - min')
        (pure 0)

turnOffGroup ::
    (KnownNat n) =>
    Records n RelayStruct ->
    Ix n ->
    Uint8 ->
    Uint32 ->
    Ivory ('Effects (Returns t) b (Scope s)) ()
turnOffGroup rs ix g t =
    arrayMap $ \jx -> do
        when (jx /=? ix) $ do
            let r = rs ! jx
            group <- deref $ r ~> group
            when (group ==? g) $ do
                isOn <- deref $ r ~> state
                delayOn' <- deref $ r ~> delayOn
                isLocked <- deref $ r ~> lock
                when (iNot isLocked) $
                    cond_
                        [ isOn ==> do
                            store (r ~> state) false
                            store (r ~> delayOn) 0
                            store (r ~> delayOff) 0
                            store (r ~> timestamp) t
                        , delayOn' >? 0 ==> do
                            store (r ~> delayOn) 0
                            store (r ~> delayOff) 0
                        ]
