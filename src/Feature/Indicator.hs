{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.Indicator where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Controller
import           Core.Domain             as D
import           Core.Feature
import           Interface.MCU
import qualified Interface.NeoPixel      as I

import           Core.Handler
import           Core.Task
import           Data.NeoPixel.Buffer
import           Data.NeoPixel.Canvas1D
import           Feature.RS485.RBUS.Data (RBUS (clock))
import           GHC.TypeNats
import           Interface.Counter
import           Interface.Mac
import           Interface.NeoPixel      (NeoPixel (transmitPixels))
import           Ivory.Language



data Indicator = forall o b. (I.NeoPixel o b, NeoPixelBuffer b) => Indicator
    { neoPixel :: o
    , canvas   :: Canvas1D 20 b
    }


indicator :: ( MonadWriter Context m
             , MonadReader (D.Domain p t) m
             , NeoPixelBuffer b
             , I.NeoPixel o b
             ) => (p -> m o) -> m Feature
indicator npx = do
    mcu      <- asks D.mcu
    neoPixel <- npx $ peripherals mcu
    canvas   <- mkCanvas1D $ I.neoPixelBuffer neoPixel "indicator"

    let indicator = Indicator { neoPixel, canvas }

    addHandler $ I.RenderNeoPixel neoPixel 60 (render indicator $ mac mcu)

    pure $ Feature indicator



render :: Indicator -> Mac -> Ivory (ProcEffects s ()) ()
render Indicator{..} mac = do
    clearCanvas canvas
    writePixel  canvas 0 (10, 20, 40)
    transmitPixels neoPixel $ getBuffer canvas



instance Controller Indicator
