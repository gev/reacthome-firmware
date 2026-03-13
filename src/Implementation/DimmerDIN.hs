{-# LANGUAGE UndecidableInstances #-}

module Implementation.DimmerDIN where

import Control.Monad.State
import Core.Actions
import Core.Context
import Core.Controller
import Core.Task
import Data.Buffer
import Data.Serialize
import Endpoint.Dimmers qualified as D
import Feature.Dimmers
import Feature.Indicator (Indicator, onFindMe)
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib
import Support.Cast

type SizeSyncStateBuff n = 1 + n

data Dimmer n = Dimmer
    { dimmers :: Dimmers n
    , indicator :: Indicator 20
    , syncStateBuff :: Buffer (SizeSyncStateBuff n) Uint8
    }

dimmer ::
    ( Monad m
    , MonadState Context m
    , KnownNat n
    , KnownNat (SizeSyncStateBuff n)
    ) =>
    (t -> m (Dimmers n)) ->
    (t -> m (Indicator 20)) ->
    m t ->
    m (Dimmer n)
dimmer dimmers' indicator' transport' = do
    transport <- transport'
    dimmers <- dimmers' transport
    indicator <- indicator' transport
    syncStateBuff <- buffer "sync_channels"

    let dimmer = Dimmer{dimmers, indicator, syncStateBuff}

    addTask $ delay 5_000 "sync_channels" $ syncChannels dimmer

    pure dimmer

instance (KnownNat n, KnownNat (SizeSyncStateBuff n)) => Controller (Dimmer n) where
    handle Dimmer{..} buff size = do
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionDo ==> onDo dimmers buff size
            , action ==? actionDim ==> onDim dimmers buff size
            , action ==? actionInitialize ==> onInit dimmers buff size
            , action ==? actionGetState ==> onGetState dimmers
            , action ==? actionFindMe ==> onFindMe indicator buff size
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