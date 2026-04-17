module Implementation.RoomNumber where

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

type ToSizeInBytes n = Div n 8 + If (Mod n 8 == 0) 0 1
type SizeSyncStateBuff n = 1 + ToSizeInBytes n

data RoomNumber n = RoomNumber
    { touches :: FT.Touches n
    , leds :: LEDs 4 5
    , buttons :: Buttons n 4 5
    , vibro :: Vibro n
    , syncStateBuff :: Buffer (SizeSyncStateBuff n) Uint8
    , info :: GetInfo
    }

roomNumber ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , Display d
    , Handler (Render (Canvas1DSize 5)) d
    , LazyTransport t
    , Flash f
    , KnownNat n
    , KnownNat (SizeSyncStateBuff n)
    ) =>
    (t -> m (FT.Touches n)) ->
    (E.DInputs n -> t -> f -> m (Vibro n)) ->
    (p -> m d) ->
    (p -> f) ->
    m t ->
    m (RoomNumber n)
roomNumber touches' vibro' display' etc' transport' = do
    transport <- transport'
    meta <- asks D.meta
    platform <- I.platform meta.mcu
    display <- display' platform.peripherals
    let etc = etc' platform.peripherals
    touches <- touches' transport
    frameBuffer <- values' "top_frame_buffer" 0
    syncStateBuff <- buffer "sync_channels"
    info <- mkGetMainInfo transport

    leds <-
        mkLeds
            frameBuffer
            [0, 1, 2, 3, 4]
            transport
            etc
            (replicate 5 true)

    ledsPerButton <-
        values
            "leds_per_button"
            [1]

    ledsOfButton <-
        matrix
            "leds_of_button"
            [ [2, 0, 0, 0]
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

    let roomNumber =
            RoomNumber
                { touches
                , leds
                , vibro
                , buttons
                , syncStateBuff
                , info
                }

    addTask $ delay 5_000 "sync_channels" $ syncChannels roomNumber

    addHandler $
        Render
            display
            30
            frameBuffer
            do
                updateLeds leds
                updateButtons buttons
                render leds

    pure roomNumber

instance (KnownNat n) => Controller (RoomNumber n) where
    handle t@RoomNumber{..} buff size = do
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

onGetState :: (KnownNat n) => RoomNumber n -> Ivory (ProcEffects s t) ()
onGetState RoomNumber{..} = do
    FT.forceSyncTouches touches
    sendVibro vibro
    sendLEDs leds

syncChannels ::
    forall n s.
    ( KnownNat n
    , KnownNat (SizeSyncStateBuff n)
    ) =>
    RoomNumber n ->
    Ivory (ProcEffects s ()) ()
syncChannels RoomNumber{..} = do
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
    FT.transmit touches syncStateBuff