module Implementation.Smart.TopA4TD where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import Core.Controller
import Core.Domain qualified as D
import Core.Handler (Handler, addHandler)
import Core.Transport
import Data.Matrix
import Data.Value
import Endpoint.DInputs as E (DInputs)
import Feature.DInputs as DI (
    DInputs (getDInputs),
    forceSyncDInputs,
 )
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
import Feature.Smart.Top.PowerTouch (PowerTouch)
import Feature.Smart.Top.Vibro (
    Vibro,
    onVibro,
    sendVibro,
 )
import GHC.TypeNats
import Interface.Display (Display, Render (Render))
import Interface.Flash
import Interface.MCU (peripherals)
import Ivory.Language
import Ivory.Stdlib

data Top n = Top
    { dinputs :: DI.DInputs n
    , leds :: LEDs 12 62
    , buttons :: Buttons n 12 62
    , vibro :: Vibro n
    , sht21 :: SHT21
    }

{- The LEDs configuration:

                            0

        5                                         1

    6     15    20 21 28    33 34 41    47 48 55
    7     14    19    27    32    40    46    54     56 61
    8  10 13    18 22 26    31 35 39    45 49 53     57 60
          12    17    25    30    38    44    52     58 59
    9     11    16 23 24    29 36 37 42 43 50 51

        4                                         2

                            3
-}

topA4TD ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , Handler (Render 186) d
    , Display d
    , LazyTransport t
    , KnownNat n
    , Flash f
    ) =>
    m t ->
    (Bool -> t -> m (DI.DInputs n)) ->
    (E.DInputs n -> t -> f -> m (Vibro n)) ->
    m PowerTouch ->
    (t -> m SHT21) ->
    (p -> m d) ->
    (p -> f) ->
    m (Top n)
topA4TD transport' dinputs' vibro' touch' sht21' display' etc' = do
    transport <- transport'
    mcu <- asks D.mcu
    display <- display' $ peripherals mcu
    dinputs <- dinputs' True transport

    frameBuffer <- values' "top_frame_buffer" 0

    let etc = etc' $ peripherals mcu

    vibro <- vibro' (DI.getDInputs dinputs) transport etc

    touch'

    leds <-
        mkLeds
            frameBuffer
            [ 0
            , 5
            , 1
            , 4
            , 2
            , 3
            , 6
            , 7
            , 8
            , 9
            , 56
            , 61
            , 57
            , 60
            , 58
            , 59
            , 15
            , 20
            , 21
            , 28
            , 33
            , 34
            , 41
            , 47
            , 48
            , 55
            , 14
            , 19
            , 27
            , 32
            , 40
            , 46
            , 54
            , 10
            , 13
            , 18
            , 22
            , 26
            , 31
            , 35
            , 39
            , 45
            , 49
            , 53
            , 12
            , 17
            , 25
            , 30
            , 38
            , 44
            , 52
            , 11
            , 16
            , 23
            , 24
            , 29
            , 36
            , 37
            , 42
            , 43
            , 50
            , 51
            ]
            transport
            etc
            (replicate 6 true)

    ledsPerButton <-
        values
            "leds_per_button"
            [2, 2, 2, 2]

    ledsOfButton <-
        matrix
            "leds_of_button"
            [ [0, 1, 0, 0]
            , [0, 2, 0, 0]
            , [3, 5, 0, 0]
            , [4, 5, 0, 0]
            ]

    buttons <-
        mkButtons
            leds
            (DI.getDInputs dinputs)
            ledsPerButton
            ledsOfButton
            transport

    sht21 <- sht21' transport

    let top =
            Top
                { dinputs
                , leds
                , vibro
                , buttons
                , sht21
                }

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
    forceSyncDInputs dinputs
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
            ]
