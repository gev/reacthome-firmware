module Feature.LeakLEDs where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Context
import Core.Domain qualified as D
import Core.Handler
import Data.Color
import Data.Display.Canvas1D hiding (canvas)
import Data.Record
import Data.Value
import GHC.TypeLits (KnownNat)
import Interface.Display hiding (display, render)
import Interface.MCU
import Ivory.Language
import Ivory.Stdlib

data LeakLEDs n = forall d. (Display d) => LeakLEDs
    { display :: d
    , canvas :: Canvas1D n
    , pixels :: Records n RGB
    , start :: Value IBool
    , t :: Value Sint32
    , stateLeak :: Value IBool
    }

mkLeakLEDs ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , Display d
    , Handler (Render (Canvas1DSize n)) d
    , KnownNat n
    , KnownNat (Canvas1DSize n)
    ) =>
    (p -> m d) -> m (LeakLEDs n)
mkLeakLEDs display' = do
    mcu <- asks D.mcu
    display <- display' $ peripherals mcu
    frameBuffer <- values' "leak_leds_frame_buffer" 0
    let canvas = mkCanvas1D frameBuffer
    pixels <- records_ "leak_leds_pixels"
    start <- value "start_leds" true
    stateLeak <- value "state_leak_leds" false
    t <- value "anima_time" 0

    addStruct (Proxy :: Proxy RGB)
    addStruct (Proxy :: Proxy HSV)

    addConstArea sinT

    let leakLEDs = LeakLEDs{display, canvas, pixels, start, t, stateLeak}

    addHandler $ Render display 25 frameBuffer do
        startAnim leakLEDs
        render leakLEDs

    pure leakLEDs

render :: (KnownNat n, KnownNat (Canvas1DSize n)) => LeakLEDs n -> Ivory (ProcEffects s ()) IBool
render LeakLEDs{..} =
    writePixels canvas pixels

setAllColorsHSV ::
    (KnownNat n) =>
    LeakLEDs n -> IFloat -> IFloat -> IFloat -> Ivory (ProcEffects s ()) ()
setAllColorsHSV LeakLEDs{..} h s v = do
    pixel <- local . istruct $ hsv h s v
    arrayMap \ix -> do
        hsv'to'rgb pixel $ pixels ! ix

startAnim :: (KnownNat n) => LeakLEDs n -> Ivory (ProcEffects s ()) ()
startAnim l@LeakLEDs{..} = do
    start' <- deref start
    when start' do
        t' <- deref t
        v <- deref $ addrOf sinT ! toIx t'
        setAllColorsHSV l 120 v v
        when (t' ==? 120) do
            store start false
        store t $ t' + 1
        stateLeak' <- deref stateLeak
        when stateLeak' do
            hasLeakLED l

sinT :: ConstMemArea (Array 120 (Stored IFloat))
sinT = constArea "led_sin_table" $ iarray $ ival . ifloat . f . fromIntegral <$> [0 .. 119]
  where
    f i = sin (pi * i / 120)

hasLeakLED :: (KnownNat n) => LeakLEDs n -> Ivory (ProcEffects s ()) ()
hasLeakLED l@LeakLEDs{..} = do
    store stateLeak true
    setAllColorsHSV l 0 1 1

hasntLeakLED :: (KnownNat n) => LeakLEDs n -> Ivory (ProcEffects s ()) ()
hasntLeakLED l@LeakLEDs{..} = do
    store stateLeak false
    setAllColorsHSV l 0 0 0
