{-# LANGUAGE UndecidableInstances #-}
module Implementation.Dimmer where

import Control.Monad.State
import Core.Actions
import Core.Context
import Core.Controller
import Core.Task
import Data.Buffer
import Data.Serialize
import Endpoint.Dimmers qualified as D
import Feature.Dimmers
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib
import Support.Cast

type SizeSyncStateBuff n = 1 + n

data Dimmer n = Dimmer
    { dimmers :: Dimmers n
    , syncStateBuff :: Buffer (SizeSyncStateBuff n) Uint8
    }

dimmer ::
    ( Monad m
    , MonadState Context m
    , KnownNat n
    , KnownNat (SizeSyncStateBuff n)
    ) =>
    m t ->
    (t -> m (Dimmers n)) ->
    m (Dimmer n)
dimmer transport' dimmers' = do
    transport <- transport'
    dimmers <- dimmers' transport
    syncStateBuff <- buffer "sync_channels"

    let dimmer = Dimmer{dimmers, syncStateBuff}

    addTask $ delay 5_000 "sync_channels" $ syncChannels dimmer

    pure dimmer

instance (KnownNat n, KnownNat (SizeSyncStateBuff n)) => Controller (Dimmer n) where
    handle d@Dimmer{..} buff size = do
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionDo ==> onDo dimmers buff size
            , action ==? actionDim ==> onDim dimmers buff size
            , action ==? actionInitialize ==> onInit dimmers buff size
            , action ==? actionGetState ==> syncChannels d
            ]

syncChannels ::
    forall n s t.
    (KnownNat n, KnownNat (SizeSyncStateBuff n)) =>
    Dimmer n ->
    Ivory (ProcEffects s t) ()
syncChannels Dimmer{..} = do
    shouldInit' <- deref $ shouldInit dimmers
    when (iNot shouldInit') do
        arrayMap \ix -> store (syncStateBuff ! ix) 0
        pack syncStateBuff 0 actionGetState
        let offset = 1
        arrayMap \ix -> do
            let dimmer = D.dimmers (getDimmers dimmers) ! ix
            dimmerBrightness <- castFloatToUint8 . (* 255) =<< deref (dimmer ~> D.brightness)
            let ixBuff = toIx . (+ offset) $ fromIx ix
            pack syncStateBuff ixBuff dimmerBrightness

        transmit dimmers syncStateBuff