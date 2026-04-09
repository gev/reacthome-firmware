{-# LANGUAGE UndecidableInstances #-}

module Implementation.DI where

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
import Endpoint.DInputs qualified as D
import Feature.DInputs (DInputs, forceSyncDInputs, getDInputs, transmit)
import Feature.DS18B20
import Feature.GetInfo
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib

type ToSizeInBytes n = Div n 8 + If (Mod n 8 == 0) 0 1
type SizeSyncStateBuff n = 1 + ToSizeInBytes n

data DI n = DI
    { dinputs :: DInputs n
    , syncStateBuff :: Buffer (SizeSyncStateBuff n) Uint8
    , info :: GetInfo
    }

di ::
    ( Monad m
    , MonadState Context m
    , KnownNat n
    , KnownNat (SizeSyncStateBuff n)
    , LazyTransport t
    , MonadReader (D.Domain p i) m
    ) =>
    (Bool -> t -> m (DInputs n)) ->
    (t -> m DS18B20) ->
    m t ->
    m (DI n)
di dinputs' ds18b20 transport' = do
    transport <- transport'
    ds18b20 transport
    dinputs <- dinputs' True transport
    syncStateBuff <- buffer "sync_channels"
    info <- mkGetInfo transport

    let di = DI{dinputs, syncStateBuff, info}

    addTask $ delay 5_000 "sync_channels" $ syncChannels di

    pure di

instance (KnownNat n, KnownNat (SizeSyncStateBuff n)) => Controller (DI n) where
    handle DI{..} buff _ = do
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionGetState ==> forceSyncDInputs dinputs
            , action ==? actionGetInfo ==> onGetInfo info
            ]

syncChannels ::
    forall n s.
    ( KnownNat n
    , KnownNat (SizeSyncStateBuff n)
    ) =>
    DI n ->
    Ivory (ProcEffects s ()) ()
syncChannels DI{..} = do
    arrayMap \ix -> store (syncStateBuff ! ix) 0
    pack syncStateBuff 0 actionGetState
    let offset = 1
    arrayMap \ix -> do
        let di' = D.dinputs (getDInputs dinputs) ! ix
        diState <- deref $ di' ~> D.state
        when diState do
            let ixByte = toIx $ offset + (fromIx ix `iDiv` 8)
            let numBit = castDefault $ fromIx ix .% 8
            byteFromBuff <- deref $ syncStateBuff ! ixByte
            let newByte = byteFromBuff .| (1 `iShiftL` numBit)
            pack syncStateBuff ixByte newByte
    transmit dinputs syncStateBuff