{-# LANGUAGE FlexibleContexts #-}

module Feature.DimmerAC where

import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Writer (MonadWriter)
import           Core.Context
import           Core.Controller
import           Core.Domain          as D
import           Core.Feature
import           Core.Handler
import           Core.Transport       as T
import           Interface.EXTI
import           Interface.MCU
import           Interface.PWM


data DimmerAC = DimmerAC

dimmerAC :: ( MonadWriter Context m
            , MonadReader (Domain p t) m
            , T.Transport t, PWM o, Handler HandleEXTI e, EXTI e
            ) => [p -> m o] -> (p -> m e) -> m Feature
dimmerAC pwms exti = do
    mcu <- asks D.mcu
    e   <- exti $ peripherals mcu
    os  <- mapM ($ peripherals mcu) pwms
    let crossZero = mapM_ resetCounter os
    addHandler $ HandleEXTI e crossZero
    pure $ Feature DimmerAC



instance Controller DimmerAC
