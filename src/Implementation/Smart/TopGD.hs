module Implementation.Smart.TopGD where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import Core.Controller
import Core.Domain qualified as D
import Core.Handler
import Core.Transport
import Data.Value
import Endpoint.DInputs as E (DInputs)
import Feature.DInputs as DI (
    DInputs (..),
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

import Core.Meta
import Core.Task
import Data.Buffer
import Data.Matrix
import Data.Serialize (pack)
import Data.Type.Bool
import Data.Type.Equality
import Endpoint.DInputs qualified as D
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
import Interface.MCU qualified as I
import Ivory.Language
import Ivory.Stdlib

type ToSizeInBytes n = Div n 8 + If (Mod n 8 == 0) 0 1
type SizeSyncStateBuff n = 1 + ToSizeInBytes n

data Top n = Top
    { dinputs :: DI.DInputs n
    , leds :: LEDs 12 64
    , buttons :: Buttons n 12 64
    , vibro :: Vibro n
    , sht21 :: SHT21
    , syncStateBuff :: Buffer (SizeSyncStateBuff n) Uint8
    }

{- The LEDs configuration:

      6 7                                        0 1

    8     17    22 23 30    35 36 43    49 50 57
    9     16    21    29    34    42    48    56     58 63
   10  12 15    20 24 28    33 37 41    47 51 55     59 62
          14    19    27    32    40    46    54     60 61
   11     13    18 25 26    31 38 39 44 45 52 53

      5 4                                        3 2
-}

topGD ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , Handler (Render 192) d
    , Display d
    , LazyTransport t
    , KnownNat n
    , Flash f
    , KnownNat (SizeSyncStateBuff n)
    ) =>
    (Bool -> t -> m (DI.DInputs n)) ->
    (E.DInputs n -> t -> f -> m (Vibro n)) ->
    m PowerTouch ->
    (t -> m SHT21) ->
    (p -> m d) ->
    (p -> f) ->
    m t ->
    m (Top n)
topGD dinputs' vibro' touch' sht21' display' etc' transport' = do
    transport <- transport'
    meta <- asks D.meta
    platform <- I.platform meta.mcu
    display <- display' platform.peripherals
    let etc = etc' platform.peripherals
    dinputs <- dinputs' True transport
    touch'
    vibro <- vibro' (DI.getDInputs dinputs) transport etc
    frameBuffer <- values' "top_frame_buffer" 0
    syncStateBuff <- buffer "sync_channels"

    leds <-
        mkLeds
            frameBuffer
            [ 6
            , 7
            , 0
            , 1
            , 5
            , 4
            , 3
            , 2
            , 8
            , 9
            , 10
            , 11
            , 58
            , 63
            , 59
            , 62
            , 60
            , 61
            , 17
            , 22
            , 23
            , 30
            , 35
            , 36
            , 43
            , 49
            , 50
            , 57
            , 16
            , 21
            , 29
            , 34
            , 42
            , 48
            , 56
            , 12
            , 15
            , 20
            , 24
            , 28
            , 33
            , 37
            , 41
            , 47
            , 51
            , 55
            , 14
            , 19
            , 27
            , 32
            , 40
            , 46
            , 54
            , 13
            , 18
            , 25
            , 26
            , 31
            , 38
            , 39
            , 44
            , 45
            , 52
            , 53
            ]
            transport
            etc
            (replicate 8 true)

    ledsPerButton <-
        values
            "leds_per_button"
            [2, 2, 2, 2, 2, 2]

    ledsOfButton <-
        matrix
            "leds_of_button"
            [ [0, 1, 0, 0]
            , [2, 3, 0, 0]
            , [4, 5, 0, 0]
            , [6, 7, 0, 0]
            , [8, 9, 0, 0]
            , [10, 11, 0, 0]
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
                , syncStateBuff
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
        let di' = D.dinputs (DI.getDInputs dinputs) ! ix
        diState <- deref $ di' ~> D.state
        when diState do
            let ixByte = toIx $ offset + (fromIx ix `iDiv` 8)
            let numBit = castDefault $ fromIx ix .% 8
            byteFromBuff <- deref $ syncStateBuff ! ixByte
            let newByte = byteFromBuff .| (1 `iShiftL` numBit)
            pack syncStateBuff ixByte newByte
    DI.transmit dinputs syncStateBuff
