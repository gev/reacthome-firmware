{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Feature.DimmerDC where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Controller
import           Core.Feature
import           Core.Transport       as T
import           Core.Domain          as D
import Interface.MCU
import Interface.PWM


data DimmerDC = forall p. PWM p => DimmerDC {pwms :: [p]}

dimmerDC :: ( MonadWriter Context m
            , MonadReader (Domain p t) m
            , T.Transport t, PWM o
            ) => [p -> m o] -> m Feature
dimmerDC pwms = do
    mcu  <- asks D.mcu 
    pwms <- mapM ($ peripherals mcu) pwms

    pure $ Feature DimmerDC {pwms}



instance Controller DimmerDC



-- x = [1,2,3]
-- z = map (+1) x


-- f = [(+1), (+2), (+3)]

-- z' = map ($ 1) f