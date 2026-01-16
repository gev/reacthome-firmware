{- HLINT ignore "Use for_" -}

module Implementation.Relay where

import Core.Actions
import Core.Context
import Core.Controller
import Core.Task
import Data.Buffer
import Data.Serialize
import Endpoint.Relays qualified as R
import Feature.Indicator (Indicator, onFindMe)
import Feature.Relays (Relays, getRelays, onDo, onGetState, onGroup, onInit, shouldInit, transmit)
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib
import Control.Monad.State
import Data.Type.Bool
import Data.Type.Equality

type SizeForSCB n = Div n 8 + If (Mod n 8 == 0) 0 1
type SCB n = 1 + SizeForSCB n

data Relay n = Relay
    { relays :: Relays n
    , indicator :: Indicator 20
    , syncChannelsBuff :: Buffer (SCB n) Uint8
    }

relay ::
    (Monad m, MonadState Context m, KnownNat n, KnownNat (SCB n)) =>
    m t ->
    (t -> m (Relays n)) ->
    (t -> m (Indicator 20)) ->
    m (Relay n)
relay transport' relays' indicator' = do
    transport <- transport'
    relays <- relays' transport
    indicator <- indicator' transport
    syncChannelsBuff <- buffer "sync_channels"

    let relay = Relay{relays, indicator, syncChannelsBuff}

    addTask $ delay 1_000 "sync_channels" $ syncChannels relay

    pure relay

syncChannels :: forall n s. (KnownNat n, KnownNat (SCB n)) => Relay n -> Ivory (ProcEffects s ()) ()
syncChannels Relay{..} = do
    shouldInit' <- deref $ shouldInit relays
    when (iNot shouldInit') do
        arrayMap \ix -> store (syncChannelsBuff ! ix) 0
        pack syncChannelsBuff 0 actionGetState

        arrayMap \ix -> do
            let relay' = R.relays (getRelays relays) ! ix
            relayState <- deref $ relay' ~> R.state
            when relayState do
                let ixByte = toIx $ 1 + (fromIx ix `iDiv` 8)
                let numBit = castDefault $ fromIx ix .% 8
                byteFromBuff <- deref $ syncChannelsBuff ! ixByte
                let newByte = byteFromBuff .| (1 `iShiftL` numBit)
                pack syncChannelsBuff ixByte newByte
        transmit relays syncChannelsBuff

instance (KnownNat n) => Controller (Relay n) where
    handle Relay{..} buff size = do
        action <- unpack buff 0
        cond_
            [ action ==? actionDo ==> onDo relays buff size
            , action ==? actionGroup ==> onGroup relays buff size
            , action ==? actionGetState ==> onGetState relays
            , action ==? actionInitialize ==> onInit relays buff size
            , action ==? actionFindMe ==> onFindMe indicator buff size
            ]
