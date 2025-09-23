{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Implementation.Doppler where

import Core.Actions
import Core.Controller
import Feature.ALED
import Feature.DInputs
import Feature.Dopplers
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib

data Doppler nd ni = Doppler
    { dopplers :: Dopplers nd
    , dinputs :: DInputs ni
    , aled :: ALED 10 100 2400
    }

doppler ::
    (Monad m) =>
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
    pure Doppler{dopplers, dinputs, aled}

onGetState Doppler{..} buff size = do
    forceSyncDInputs dinputs
    forceSyncAled aled

instance (KnownNat ni) => Controller (Doppler nd ni) where
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
