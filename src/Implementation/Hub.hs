{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Implementation.Hub where

import Control.Monad.Reader (MonadReader, asks)
import Core.Actions
import Core.Controller (Controller, handle)
import qualified Core.Domain as D
import Data.Buffer
import Data.Fixed
import Data.Serialize
import Data.Value
import qualified Endpoint.ALED as E
import Endpoint.Dimmers as Dim (
    dimmers,
    initialize,
    syncDimmerGroup,
 )
import Feature.ALED
import Feature.DInputs (DInputs, forceSyncDInputs)
import Feature.DS18B20 (DS18B20)
import Feature.Dimmers (
    Dimmers (getDimmers),
    forceSync,
    n,
    onDim,
    onDo,
 )
import Feature.Indicator (Indicator, onFindMe)
import Feature.RS485.RBUS (
    configureMode,
    forceSyncRBUS',
    setMode,
    transmitRB485,
    transmitRBUS,
 )
import Feature.RS485.RBUS.Data (RBUS (..))
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib

data Hub ni nd nr = Hub
    { rbus :: List nr RBUS
    , dimmers :: Dimmers nd
    , dinputs :: DInputs ni
    , indicator :: Indicator 20
    , aled :: ALED 10 100 2400
    , shouldInit :: Value IBool
    }

hub ::
    (MonadReader (D.Domain p c) m) =>
    m t ->
    (t -> m (List nr RBUS)) ->
    (t -> m (Dimmers nd)) ->
    (Bool -> t -> m (DInputs ni)) ->
    (t -> m DS18B20) ->
    (t -> m (Indicator 20)) ->
    (t -> m (ALED 10 100 2400)) ->
    m (Hub ni nd nr)
hub transport' rbus' dimmers' dinputs' ds18b20' indicator' aled' = do
    transport <- transport'
    rbus <- rbus' transport
    dimmers <- dimmers' transport
    dinputs <- dinputs' True transport
    indicator <- indicator' transport
    aled <- aled' transport
    shouldInit <- asks D.shouldInit
    ds18b20' transport
    pure Hub{rbus, dimmers, dinputs, indicator, shouldInit, aled}

instance (KnownNat ni, KnownNat nd, KnownNat nr) => Controller (Hub ni nd nr) where
    handle s@Hub{..} buff size = do
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionDo ==> onDo dimmers buff size
            , action ==? actionDim ==> onDim dimmers buff size
            , action ==? actionRs485Mode ==> setMode rbus buff size
            , action ==? actionRbusTransmit ==> transmitRBUS rbus buff size
            , action ==? actionRs485Transmit ==> transmitRB485 rbus buff size
            , action ==? actionFindMe ==> onFindMe indicator buff size
            , action ==? actionInitialize ==> onInit s buff size
            , action ==? actionGetState ==> onGetState s
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

onInit ::
    (KnownNat l, KnownNat nd) =>
    Hub ni nd nr ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onInit Hub{..} buff size = do
    let s = 1 + (6 * fromIntegral (length rbus))
    let dim' = n dimmers * 3
    let ng' = 10
    when (size ==? s + dim' + ng') $ do
        let run r@RBUS{..} offset = do
                store mode =<< unpack buff offset
                store baudrate =<< unpackLE buff (offset + 1)
                store lineControl =<< unpack buff (offset + 5)
                configureMode r
        zipWithM_ run rbus $ fromIntegral <$> fromList [1, 7 ..]

        offset <- local $ ival $ toIx s

        let ds = Dim.dimmers $ getDimmers dimmers
        arrayMap $ \ix -> do
            offset' <- deref offset
            let d = ds ! ix
            group <- unpack buff offset'
            mode <- unpack buff (offset' + 1)
            value <- unpack buff (offset' + 2) :: Ivory eff Uint8
            initialize d group mode (safeCast value / 255) 0
            syncDimmerGroup ds d ix
            store offset $ offset' + 3

        arrayMap $ \ix -> do
            offset' <- deref offset
            let group = E.groups (getALED aled) ! ix
            brightness <- deref $ buff ! offset'
            store (group ~> E.brightness) $ safeCast brightness / 255
            store offset $ offset' + 1

        store shouldInit false

onGetState Hub{..} = do
    forceSyncDInputs dinputs
    initialized <- iNot <$> deref shouldInit
    when initialized $ do
        forceSync dimmers
        forceSyncRBUS' rbus
