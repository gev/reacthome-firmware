{-# LANGUAGE FlexibleContexts #-}

module Feature.MixA where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Core.Transport       as T


data MixA = MixA

mixA :: ( MonadWriter Context m
        , MonadReader (Domain p t) m
        , T.Transport t
        ) => [p -> m i] -> [p -> m o] -> m Feature
mixA inputs outputs = pure $ Feature MixA



instance Controller MixA
