{- HLINT ignore "Use for_" -}
{-# LANGUAGE UndecidableInstances #-}

module Implementation.Relay where

import Control.Monad.Reader
import Control.Monad.State
import Core.Actions
import Core.Context
import Core.Controller
import Core.Domain qualified as D
import Core.Task
import Core.Transport
import Data.Buffer
import Data.Serialize
import Data.Type.Bool
import Data.Type.Equality
import Endpoint.Relays qualified as E
import Feature.GetInfo
import Feature.Indicator (Indicator, onFindMe)
import Feature.Relays (Relays, getRelays, onDo, onGetState, onGroup, onInit, shouldInit, transmit)
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib

type ToSizeInBytes n = Div n 8 + If (Mod n 8 == 0) 0 1
type SizeSyncStateBuff n = 1 + ToSizeInBytes n

data Relay n = Relay
    { relays :: Relays n
    , indicator :: Indicator 20
    , syncStateBuff :: Buffer (SizeSyncStateBuff n) Uint8
    , info :: GetInfo
    }

relay ::
    ( Monad m
    , MonadState Context m
    , KnownNat n
    , KnownNat (SizeSyncStateBuff n)
    , LazyTransport t
    , MonadReader (D.Domain p c) m
    ) =>
    (t -> m (Relays n)) ->
    (t -> m (Indicator 20)) ->
    m t ->
    m (Relay n)
relay relays' indicator' transport' = do
    transport <- transport'
    relays <- relays' transport
    indicator <- indicator' transport
    syncStateBuff <- buffer "sync_channels"
    info <- mkGetMainInfo transport

    let relay = Relay{relays, indicator, syncStateBuff, info}

    addTask $ delay 1_000 "sync_channels" $ syncChannels relay

    pure relay

instance (KnownNat n, KnownNat (SizeSyncStateBuff n)) => Controller (Relay n) where
    handle Relay{..} buff size = do
        action <- unpack buff 0
        cond_
            [ action ==? actionDo ==> onDo relays buff size
            , action ==? actionGroup ==> onGroup relays buff size
            , action ==? actionGetState ==> onGetState relays
            , action ==? actionInitialize ==> onInit relays buff size
            , action ==? actionFindMe ==> onFindMe indicator buff size
            , action ==? actionGetInfo ==> onGetInfo info
            ]

syncChannels ::
    forall n s t.
    (KnownNat n, KnownNat (SizeSyncStateBuff n)) =>
    Relay n ->
    Ivory (ProcEffects s t) ()
syncChannels Relay{..} = do
    shouldInit' <- deref $ shouldInit relays
    when (iNot shouldInit') do
        arrayMap \ix -> store (syncStateBuff ! ix) 0
        pack syncStateBuff 0 actionGetState

        arrayMap \ix -> do
            let relay' = E.relays (getRelays relays) ! ix
            relayState <- deref $ relay' ~> E.state
            when relayState do
                let ixByte = toIx $ 1 + (fromIx ix `iDiv` 8)
                let numBit = castDefault $ fromIx ix .% 8
                byteFromBuff <- deref $ syncStateBuff ! ixByte
                let newByte = byteFromBuff .| (1 `iShiftL` numBit)
                pack syncStateBuff ixByte newByte
        transmit relays syncStateBuff