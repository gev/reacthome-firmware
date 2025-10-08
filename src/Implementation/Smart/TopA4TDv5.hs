module Implementation.Smart.TopA4TDv5 where

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
import Feature.Touches qualified as FT
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
    { touches :: FT.Touches n
    , leds :: LEDs 12 41
    , buttons :: Buttons n 12 41
    , vibro :: Vibro n
    , sht21 :: SHT21
    }

{- The LEDs configuration:

                            0

        5                                         1

    6              15          22          30   
    7     12    14    19    21    26    29    34     35 40
    8  10          16          23          31        36 39
          11    13    18    20    25    28    33     37 38
    9              17          24    27    32   

        4                                         2

                            3
-}

topA4TDv5 ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , Handler (Render 123) d
    , Display d
    , LazyTransport t
    , KnownNat n
    , Flash f
    ) =>
    m t ->
    (t -> m (FT.Touches n)) ->
    (E.DInputs n -> t -> f -> m (Vibro n)) ->
    (t -> m SHT21) ->
    (p -> m d) ->
    (p -> f) ->
    m (Top n)
topA4TDv5 transport' touches' vibro' sht21' display' etc' = do
    transport <- transport'
    mcu <- asks D.mcu
    display <- display' $ peripherals mcu
    touches <- touches' transport

    frameBuffer <- values' "top_frame_buffer" 0

    let etc = etc' $ peripherals mcu

    vibro <- vibro' (FT.getDInputs touches) transport etc

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
            , 35
            , 40
            , 36
            , 39
            , 37
            , 38
            , 15
            , 22
            , 30 
            , 12
            , 14
            , 19
            , 21
            , 26
            , 29
            , 34
            , 10
            , 16
            , 23
            , 31
            , 11
            , 13
            , 18
            , 20
            , 25
            , 28
            , 33
            , 17
            , 24
            , 27
            , 32
            ]
            transport
            etc
            (replicate 41 true)

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
            (FT.getDInputs touches)
            ledsPerButton
            ledsOfButton
            transport

    sht21 <- sht21' transport

    let top =
            Top
                { touches
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
            ]
