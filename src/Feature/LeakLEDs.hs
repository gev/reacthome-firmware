{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.LeakLEDs where

import           Control.Monad.Reader  (MonadReader, asks)
import           Control.Monad.State   (MonadState)
import           Core.Context
import qualified Core.Domain           as D
import           Core.Handler
import           Data.Color
import           Data.Display.Canvas1D hiding (canvas)
import           Data.Record
import           Data.Value
import           GHC.TypeLits          (KnownNat)
import           Interface.Display     hiding (display, render)
import           Interface.MCU
import           Ivory.Language
import           Ivory.Stdlib          (when)


data LeakLEDs n = forall d. Display d => LeakLEDs
                 { display :: d
                 , canvas  :: Canvas1D n
                 , pixels  :: Records  n RGB
                 , start   :: Value IBool
                 , t       :: Value Sint32
                 }


mkLeakLEDs ::( MonadState Context m
             , MonadReader (D.Domain p c) m
             , Display d, Handler (Render (Canvas1DSize n)) d
             , KnownNat n, KnownNat (Canvas1DSize n)
             ) => (p -> m d) -> m (LeakLEDs n)
mkLeakLEDs display' = do
    mcu          <- asks D.mcu
    display      <- display' $ peripherals mcu
    frameBuffer  <- values'    "leak_leds_frame_buffer" 0
    let canvas   = mkCanvas1D  frameBuffer
    pixels       <- records_   "leak_leds_pixels"
    start        <- value      "start_leds" true
    t            <- value      "anima_time" 0

    addStruct   (Proxy :: Proxy RGB)
    addStruct   (Proxy :: Proxy HSV)

    addConstArea sinT

    let leakLEDs = LeakLEDs { display, canvas, pixels, start, t}

    addHandler $ Render display 25 frameBuffer $ do
        startAnim leakLEDs
        render leakLEDs

    pure leakLEDs



render :: (KnownNat n, KnownNat (Canvas1DSize n)) => LeakLEDs n -> Ivory (ProcEffects s ()) IBool
render LeakLEDs{..} =
    writePixels canvas pixels


setAllColorsHSV :: KnownNat n =>
                   LeakLEDs n -> IFloat -> IFloat -> IFloat -> Ivory (ProcEffects s ()) ()
setAllColorsHSV LeakLEDs{..} h s v = do
    pixel  <- local . istruct $ hsv h s v
    arrayMap $ \ix -> do
        hsv'to'rgb pixel $ pixels ! ix


startAnim :: KnownNat n => LeakLEDs n  -> Ivory (ProcEffects s ()) ()
startAnim d@LeakLEDs{..} = do
    start' <- deref start
    when start' $ do
        t' <- deref t
        v  <- deref $ addrOf sinT ! toIx t'
        setAllColorsHSV d 120 v v
        when (t' ==? 120)
             (store start false)
        store t $ t' + 1


sinT :: ConstMemArea (Array 120 (Stored IFloat))
sinT = constArea "led_sin_table" $ iarray $ ival . ifloat . f . fromIntegral <$> [0..119]
    where f i = sin (pi * i / 120)
