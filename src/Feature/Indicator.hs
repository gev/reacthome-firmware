{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeOperators    #-}

module Feature.Indicator where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Controller
import           Core.Domain          as D
import           Core.Feature
import           Interface.MCU
import           Interface.NeoPixel   as I

import           Core.Task
import           Data.NeoPixel.Buffer
import           GHC.TypeNats
import           Ivory.Language



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



indicatorTask pixels mac =
    arrayMap $ \ix ->
        writeByte pixels (toIx $ fromIx ix) =<< deref (mac ! ix)



instance Controller Indicator
