{-# LANGUAGE UndecidableInstances #-}

module Implementation.DI where

import Control.Monad.State
import Core.Actions
import Core.Context
import Core.Controller
import Data.Buffer
import Data.Serialize
import Data.Type.Bool
import Data.Type.Equality
import Endpoint.DInputs qualified as D
import Feature.DInputs (DInputs, getDInputs, transmit, forceSyncDInputs)
import Feature.DS18B20
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib
import Core.Task

type ToSizeInBytes n = Div n 8 + If (Mod n 8 == 0) 0 1
type SizeSyncStateBuff n = 1 + ToSizeInBytes n

data DI n = DI
    { dinputs :: DInputs n
    , syncStateBuff :: Buffer (SizeSyncStateBuff n) Uint8
    }

di ::
    ( Monad m
    , MonadState Context m
    , KnownNat n
    , KnownNat (SizeSyncStateBuff n)
    ) =>
    m t ->
    (Bool -> t -> m (DInputs n)) ->
    (t -> m DS18B20) ->
    m (DI n)
di transport' dinputs' ds18b20 = do
    transport <- transport'
    ds18b20 transport
    dinputs <- dinputs' True transport
    syncStateBuff <- buffer "sync_channels"

    let di = DI{dinputs, syncStateBuff}

    addTask $ delay 5_000 "sync_channels" $ syncChannels di    

    pure di

instance (KnownNat n, KnownNat (SizeSyncStateBuff n)) => Controller (DI n) where
    handle DI{..} buff _ = do
        action <- deref $ buff ! 0
        cond_
            [ action
                ==? actionGetState
                ==> forceSyncDInputs dinputs
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