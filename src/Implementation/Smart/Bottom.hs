{-# LANGUAGE UndecidableInstances #-}
module Implementation.Smart.Bottom where

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
import Feature.DS18B20
import Feature.Scd40 hiding (transmit)
import Feature.Smart.Top
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib

type ToSizeInBytes n = Div n 8 + If (Mod n 8 == 0) 0 1
type SizeSyncStateBuff n = 1 + ToSizeInBytes n

data Bottom n = Bottom
    { top :: Top
    , dinputs :: DInputs n
    , aled :: ALED 10 100 2040
    , syncStateBuff :: Buffer (SizeSyncStateBuff n) Uint8
    }

bottom1 ::
    ( MonadState Context m
    , KnownNat n
    , Monad m
    , KnownNat (SizeSyncStateBuff n)
    ) =>
    m t ->
    (t -> m Top) ->
    (Bool -> t -> m (DInputs n)) ->
    (t -> m DS18B20) ->
    (t -> m (ALED 10 100 2040)) ->
    m (Bottom n)
bottom1 transport' top' dinputs' ds18b20 aled' = do
    transport <- transport'
    ds18b20 transport
    dinputs <- dinputs' True transport
    top <- top' transport
    aled <- aled' transport
    syncStateBuff <- buffer "sync_channels"

    let bottom =
            Bottom
                { top
                , dinputs
                , aled
                , syncStateBuff
                }

    addTask $ delay 5_000 "sync_channels" $ syncChannels bottom

    pure bottom

bottom2 ::
    ( MonadState Context m
    , KnownNat n
    , Monad m
    , KnownNat (SizeSyncStateBuff n)
    ) =>
    m t ->
    (t -> m Top) ->
    ( Bool ->
      t ->
      m (DInputs n)
    ) ->
    (t -> m DS18B20) ->
    (t -> m SCD40) ->
    (t -> m (ALED 10 100 2040)) ->
    m (Bottom n)
bottom2 transport top dinputs ds18b20 scd40 aled' = do
    scd40 =<< transport
    bottom1
        transport
        top
        dinputs
        ds18b20
        aled'

onGetState Bottom{..} _ _ = do
    forceSyncDInputs dinputs
    forceSyncTop top
    forceSyncAled aled

instance (KnownNat n, KnownNat (SizeSyncStateBuff n)) => Controller (Bottom n) where
    handle b@Bottom{..} buff size = do
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionGetState ==> onGetState b buff size
            , action ==? actionSmartTop ==> onMessage top buff size
            , action ==? actionFindMe ==> onFindMe top buff size
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
    forall n s t.
    ( KnownNat n
    , KnownNat (SizeSyncStateBuff n)
    ) =>
    Bottom n ->
    Ivory (ProcEffects s t) ()
syncChannels Bottom{..} = do
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