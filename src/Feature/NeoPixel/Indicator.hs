{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeOperators   #-}

module Feature.NeoPixel.Indicator where

import           Control.Monad.Reader 
import           Control.Monad.Writer 
import Core.Feature
import Core.Domain as D
import Core.Context
import Core.Controller
import Interface.NeoPixel as I
import           Interface.MCU

import Data.NeoPixel.Buffer
import           GHC.TypeNats
import Ivory.Language
import Core.Task



data Indicator = forall b. NeoPixelBuffer b => Indicator 
    { pixels :: b 60

    }


indicator :: ( MonadWriter Context m
             , MonadReader (D.Domain p t) m
             , NeoPixelBuffer b
             , I.NeoPixel o b
             ) => (p -> m o) -> m Feature
indicator npx = do
    mcu      <- asks D.mcu
    neoPixel <- npx $ peripherals mcu
    pixels   <- neoPixelBuffer neoPixel "indicator"

    let initIndicator' :: Def ('[] :-> ())
        initIndicator' = proc "indicator_init" $ body $ transmitPixels neoPixel pixels

    addInit initIndicator'

    addTask $ delay 100 "indicator_task" $ indicatorTask pixels $ mac mcu

    pure $ Feature Indicator { pixels }



indicatorTask pixels mac = undefined


instance Controller Indicator
