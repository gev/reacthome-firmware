module Implementation.Smart.TopCardHolder where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import Core.Controller
import Core.Domain qualified as D
import Core.Handler
import Core.Transport
import Data.Display.Canvas1D (Canvas1DSize)
import Data.Value
import Endpoint.DInputs as E (DInputs)
import Feature.Smart.Top.Buttons
import Feature.Smart.Top.LEDs (
    LEDs,
    mkLeds,
    onBlink,
    onDim,
    onDo,
    onImage,
    onPalette,
    onSetColor,
    render,
    sendLEDs,
    updateLeds,
 )

import Core.Meta
import Core.Task
import Data.Buffer
import Data.Matrix
import Data.Serialize
import Data.Type.Bool
import Data.Type.Equality
import Endpoint.DInputs qualified as D
import Feature.DInputs qualified as FDI
import Feature.GetInfo
import Feature.Smart.Top.Vibro (
    Vibro,
    onVibro,
    sendVibro,
 )
import Feature.Touches qualified as FT
import GHC.TypeNats
import Interface.Display (Display, Render (Render))
import Interface.Flash
import Interface.MCU (peripherals)
import Interface.MCU qualified as I
import Ivory.Language
import Ivory.Stdlib
import Ivory.Language.Proxy

type ToSizeInBytes n = Div n 8 + If (Mod n 8 == 0) 0 1
type SizeSyncStateBuff n = 1 + ToSizeInBytes n

data Top nt nd = Top
    { touches :: FT.Touches nt
    , dinputs :: FDI.DInputs nd
    , leds :: LEDs 4 3
    , buttons :: Buttons nt 4 3
    , vibro :: Vibro nt
    , syncStateBuff :: Buffer (SizeSyncStateBuff nt) Uint8
    , info :: GetInfo
    }

topCardHolder ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , Display d
    , Handler (Render (Canvas1DSize 3)) d
    , LazyTransport t
    , Flash f
    , KnownNat nt
    , KnownNat nd
    , KnownNat (SizeSyncStateBuff nt)
    ) =>
    (t -> m (FT.Touches nt)) ->
    (Bool -> Uint8 -> t -> m (FDI.DInputs nd)) ->
    (E.DInputs nt -> t -> f -> m (Vibro nt)) ->
    (p -> m d) ->
    (p -> f) ->
    m t ->
    m (Top nt nd)
topCardHolder touches' dinputs' vibro' display' etc' transport' = do
    transport <- transport'
    meta <- asks D.meta
    platform <- I.platform meta.mcu
    display <- display' platform.peripherals
    let etc = etc' platform.peripherals
    touches <- touches' transport
    dinputs <- dinputs' True 2 transport
    frameBuffer <- values' "top_frame_buffer" 0
    syncStateBuff <- buffer "sync_channels"
    info <- mkGetMainInfo transport

    leds <-
        mkLeds
            frameBuffer
            [0, 1, 2]
            transport
            etc
            (replicate 3 true)

    ledsPerButton <-
        values
            "leds_per_button"
            [1, 1]

    ledsOfButton <-
        matrix
            "leds_of_button"
            [ [0, 0, 0, 0]
            , [2, 0, 0, 0]
            ]

    buttons <-
        mkButtons
            leds
            (FT.getDInputs touches)
            ledsPerButton
            ledsOfButton
            transport

    vibro <-
        vibro'
            (FT.getDInputs touches)
            transport
            etc

    let top =
            Top
                { touches
                , dinputs
                , leds
                , vibro
                , buttons
                , syncStateBuff
                , info
                }

    addTask $ delay 5_000 "sync_channels" $ syncChannels top

    addHandler $
        Render
            display
            30
            frameBuffer
            do
                updateLeds leds
                updateButtons buttons
                render leds

    pure top

onGetState :: (KnownNat nt, KnownNat nd) => Top nt nd -> Ivory (ProcEffects s t) ()
onGetState Top{..} = do
    FT.forceSyncTouches touches
    FDI.forceSyncDInputs dinputs
    sendVibro vibro
    sendLEDs leds

instance (KnownNat nt) => Controller (Top nt nd) where
    handle t@Top{..} buff size = do
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionDo ==> onDo leds buff size
            , action ==? actionDim ==> onDim leds buff size
            , action ==? actionRGB ==> onSetColor leds buff size
            , action ==? actionImage ==> onImage leds buff size
            , action ==? actionBlink ==> onBlink leds buff size
            , action ==? actionPalette ==> onPalette leds buff size
            , action ==? actionVibro ==> onVibro vibro buff size
            , action ==? actionFindMe ==> onFindMe buttons buff size
            , action ==? actionGetState ==> onGetState t
            , action ==? actionGetInfo ==> onGetInfo info
            ]

syncChannels ::
    forall nt nd s.
    ( KnownNat nt
    , KnownNat nd
    , KnownNat (SizeSyncStateBuff nt)
    ) =>
    Top nt nd ->
    Ivory (ProcEffects s ()) ()
syncChannels Top{..} = do
    arrayMap \ix -> store (syncStateBuff ! ix) 0
    pack syncStateBuff 0 actionGetState
    let offset = 1
    arrayMap \ix -> do
        let di' = D.dinputs (FT.getDInputs touches) ! ix
        diState <- deref $ di' ~> D.state
        when diState do
            let ixByte = toIx $ offset + (fromIx ix `iDiv` 8)
            let numBit = castDefault $ fromIx ix .% 8
            byteFromBuff <- deref $ syncStateBuff ! ixByte
            let newByte = byteFromBuff .| (1 `iShiftL` numBit)
            pack syncStateBuff ixByte newByte
    let offsetBit = fromIntegral $ natVal (aNat :: NatType nt)
    arrayMap \ix -> do
        let di' = D.dinputs (FDI.getDInputs dinputs) ! ix
        diState <- deref $ di' ~> D.state
        when diState do
            let ixByte = toIx $ offset + (fromIx ix `iDiv` 8)
            let numBit = castDefault $ fromIx ix .% 8 + offsetBit
            byteFromBuff <- deref $ syncStateBuff ! ixByte
            let newByte = byteFromBuff .| (1 `iShiftL` numBit)
            pack syncStateBuff ixByte newByte
    FT.transmit touches syncStateBuff
