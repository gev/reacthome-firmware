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
import           Feature.RS485.RBUS.Data (RBUS (clock))
import           GHC.TypeNats
import           Interface.Counter
import           Interface.Mac
import           Interface.NeoPixel      (NeoPixel (transmitPixels))
import           Ivory.Language



data Indicator = forall o b. (I.NeoPixel o b, NeoPixelBuffer b) => Indicator
    { neoPixel :: o
    , pixels   :: b 60
    }


indicator :: ( MonadWriter Context m
             , MonadReader (D.Domain p t) m
             , NeoPixelBuffer b
             , I.NeoPixel o b
             ) => (p -> m o) -> m Feature
indicator npx = do
    mcu      <- asks D.mcu
    neoPixel <- npx $ peripherals mcu
    pixels   <- I.neoPixelBuffer neoPixel "indicator"

    let indicator = Indicator { neoPixel, pixels }

    addHandler $ I.RenderNeoPixel neoPixel 10 (render indicator $ mac mcu)

    pure $ Feature indicator



render :: Indicator -> Mac -> Ivory (ProcEffects s ()) ()
render Indicator{..} mac = do
    clearBuffer pixels
    writeByte pixels 1 1
    arrayMap $ \ix -> do
        v <- deref (mac ! ix)
        writeByte pixels (toIx $ fromIx ix) v
    transmitPixels neoPixel pixels



instance Controller Indicator
