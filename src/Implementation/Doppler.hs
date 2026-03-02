{-# LANGUAGE UndecidableInstances #-}

module Implementation.Doppler where

import Control.Monad.State
import Core.Actions
import Core.Context
import Core.Controller
import Core.Task
import Data.Buffer
import Data.Serialize
import Data.Type.Bool
import Data.Type.Equality
import Endpoint.DInputs qualified as D
import Feature.ALED
import Feature.DInputs
import Feature.Dopplers
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib

type ToSizeInBytes n = Div n 8 + If (Mod n 8 == 0) 0 1
type SizeSyncStateBuff n = 1 + ToSizeInBytes n

data Doppler nd ni = Doppler
    { dopplers :: Dopplers nd
    , dinputs :: DInputs ni
    , aled :: ALED 10 100 2400
    , syncStateBuff :: Buffer (SizeSyncStateBuff ni) Uint8
    }

doppler ::
    ( Monad m
    , KnownNat nd
    , KnownNat ni
    , MonadState Context m
    , KnownNat (SizeSyncStateBuff ni)
    ) =>
    m t ->
    (t -> m (Dopplers nd)) ->
    (Bool -> t -> m (DInputs ni)) ->
    (t -> m (ALED 10 100 2400)) ->
    m (Doppler nd ni)
doppler transport' dopplers' dinputs' aled' = do
    transport <- transport'
    dopplers <- dopplers' transport
    dinputs <- dinputs' True transport
    aled <- aled' transport
    syncStateBuff <- buffer "sync_channels"

    let doppler = Doppler{dopplers, dinputs, aled, syncStateBuff}

    addTask $ delay 5_000 "sync_channels" $ syncChannels doppler

    pure doppler

onGetState Doppler{..} _ _ = do
    forceSyncDInputs dinputs
    forceSyncAled aled

instance
    ( KnownNat nd
    , KnownNat ni
    , KnownNat (SizeSyncStateBuff ni)
    ) =>
    Controller (Doppler nd ni)
    where
    handle d@Doppler{..} buff size = do
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionGetState ==> onGetState d buff size
            , action ==? actionInitialize ==> onInitialize aled buff size
            , action ==? actionALedOn ==> onALedOn aled buff size
            , action ==? actionALedOff ==> onALedOff aled buff size
            , action ==? actionALedColorAnimationPlay ==> onALedColorAnimationPlay aled buff size
            , action ==? actionALedColorAnimationStop ==> onALedColorAnimationStop aled buff size
            , action ==? actionALedMaskAnimationPlay ==> onALedMaskAnimationPlay aled buff size
            , action ==? actionALedMaskAnimationStop ==> onALedMaskAnimationStop aled buff size
            , action ==? actionALedClip ==> onALedClip aled buff size
            , action ==? actionALedBrightness ==> onALedBrightness aled buff size
            , action ==? actionALedConfigGroup ==> onALedConfigGroup aled buff size
            ]

syncChannels ::
    forall nd ni s t.
    ( KnownNat nd
    , KnownNat ni
    , KnownNat (SizeSyncStateBuff ni)
    ) =>
    Doppler nd ni ->
    Ivory (ProcEffects s t) ()
syncChannels Doppler{..} = do
    arrayMap \ix -> store (syncStateBuff ! ix) 0
    pack syncStateBuff 0 actionGetState
    let offset = 1
    arrayMap \ix -> do
        let relay' = D.dinputs (getDInputs dinputs) ! ix
        diState <- deref $ relay' ~> D.state
        when diState do
            let ixByte = toIx $ offset + (fromIx ix `iDiv` 8)
            let numBit = castDefault $ fromIx ix .% 8
            byteFromBuff <- deref $ syncStateBuff ! ixByte
            let newByte = byteFromBuff .| (1 `iShiftL` numBit)
            pack syncStateBuff ixByte newByte
    transmit dinputs syncStateBuff
