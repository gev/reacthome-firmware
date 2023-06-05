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
import           Data.Color
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
    , color    :: RGB
    }


indicator :: ( MonadWriter Context m
             , MonadReader (D.Domain p t) m
             , NeoPixelBuffer b
             , I.NeoPixel o b
             ) => (p -> m o) -> RGB -> m Feature
indicator npx color = do
    mcu      <- asks D.mcu
    neoPixel <- npx $ peripherals mcu
    canvas   <- mkCanvas1D $ I.neoPixelBuffer neoPixel "indicator"

    let indicator = Indicator { neoPixel, canvas, color }

    addHandler $ I.RenderNeoPixel neoPixel 60 (render indicator)

    pure $ Feature indicator



render :: Indicator -> Ivory (ProcEffects s ()) ()
render Indicator{..} = do
    clearCanvas canvas
    arrayMap $ \ix -> writePixel canvas ix color
    transmitPixels neoPixel $ getBuffer canvas



instance Controller Indicator
