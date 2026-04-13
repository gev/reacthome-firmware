module Implementation.Smart.TopG4v9 where

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
import Feature.Sht21 (SHT21)
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

data Top n = Top
    { touches :: FT.Touches n
    , leds :: LEDs 4 12
    , buttons :: Buttons n 4 12
    , vibro :: Vibro n
    , sht21 :: SHT21
    , syncStateBuff :: Buffer (SizeSyncStateBuff n) Uint8
    , info :: GetInfo
    }

topG4v9 ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , Display d
    , Handler (Render (Canvas1DSize 12)) d
    , LazyTransport t
    , Flash f
    , KnownNat n
    , KnownNat (SizeSyncStateBuff n)
    ) =>
    (t -> m (FT.Touches n)) ->
    (E.DInputs n -> t -> f -> m (Vibro n)) ->
    (t -> m SHT21) ->
    (p -> m d) ->
    (p -> f) ->
    m t ->
    m (Top n)
topG4v9 touches' vibro' sht21' display' etc' transport' = do
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
            [10, 11, 0, 1, 7, 6, 5, 4, 8, 9, 2, 3]
            transport
            etc
            (replicate 8 true <> replicate 4 false)

    ledsPerButton <-
        values
            "leds_per_button"
            [2, 2, 2, 2]

    ledsOfButton <-
        matrix
            "leds_of_button"
            [ [0, 1, 0, 0]
            , [2, 3, 0, 0]
            , [4, 5, 0, 0]
            , [6, 7, 0, 0]
            ]

    buttons <-
        mkButtons
            leds
            (FT.getDInputs touches)
            ledsPerButton
            ledsOfButton
            transport
    sht21 <- sht21' transport

    vibro <-
        vibro'
            (FT.getDInputs touches)
            transport
            etc

    let top =
            Top
                { touches
                , leds
                , vibro
                , buttons
                , sht21
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

onGetState :: (KnownNat n) => Top n -> Ivory (ProcEffects s t) ()
onGetState Top{..} = do
    FT.forceSyncTouches touches
    sendVibro vibro
    sendLEDs leds

instance (KnownNat n) => Controller (Top n) where
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
    forall n s.
    ( KnownNat n
    , KnownNat (SizeSyncStateBuff n)
    ) =>
    Top n ->
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
    FT.transmit touches syncStateBuff
