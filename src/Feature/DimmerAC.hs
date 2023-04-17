{-# LANGUAGE FlexibleContexts #-}

module Feature.DimmerAC where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Core.Transport       as T


data DimmerAC = DimmerAC

dimmerAC :: ( MonadWriter Context m
            , MonadReader (Domain p t) m
            , T.Transport t
            ) => [p -> m o] -> (p -> m i) -> m Feature
dimmerAC outs input = pure $ Feature DimmerAC



instance Controller DimmerAC
