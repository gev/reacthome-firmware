{-# LANGUAGE FlexibleContexts #-}

module Feature.DimmerDC where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Core.Transport       as T


data DimmerDC = DimmerDC

dimmerDC :: ( MonadWriter Context m
            , MonadReader (Domain p t) m
            , T.Transport t
            ) => [p -> m o] -> m Feature
dimmerDC outs = pure $ Feature DimmerDC



instance Controller DimmerDC
