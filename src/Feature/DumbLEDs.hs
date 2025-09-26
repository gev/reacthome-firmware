{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.DumbLEDs where

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


data DumbLEDs n = forall d. Display d => DumbLEDs
                 { display :: d
                 , canvas  :: Canvas1D n
                 , pixels  :: Records  n RGB
                 }


mkDumbLEDs ::( MonadState Context m
             , MonadReader (D.Domain p c) m
             , Display d, Handler (Render (Canvas1DSize n)) d
             , KnownNat n, KnownNat (Canvas1DSize n)
             ) => (p -> m d) -> m (DumbLEDs n)
mkDumbLEDs display' = do
    mcu          <- asks D.mcu
    display      <- display' $ peripherals mcu
    frameBuffer  <- values'    "dumb_leds_frame_buffer" 0
    let canvas   = mkCanvas1D  frameBuffer
    pixels       <- records_   "dumb_leds_pixels"

    addStruct   (Proxy :: Proxy RGB)
    addStruct   (Proxy :: Proxy HSV)

    let dumbLEDs = DumbLEDs { display, canvas, pixels}

    addHandler $ Render display 25 frameBuffer $ do
        render dumbLEDs

    pure dumbLEDs



render :: (KnownNat n, KnownNat (Canvas1DSize n)) => DumbLEDs n -> Ivory (ProcEffects s ()) IBool
render DumbLEDs{..} =
    writePixels canvas pixels


setAllColorsHSV :: KnownNat n =>
                   DumbLEDs n -> IFloat -> IFloat -> IFloat -> Ivory (ProcEffects s ()) ()
setAllColorsHSV DumbLEDs{..} h s v = do
    pixel  <- local . istruct $ hsv h s v
    arrayMap $ \ix -> do
        hsv'to'rgb pixel $ pixels ! ix

