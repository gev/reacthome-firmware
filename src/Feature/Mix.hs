{-# LANGUAGE FlexibleContexts #-}

module Feature.Mix where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Core.Transport       as T


data Mix = Mix

mix :: ( MonadWriter Context m
        , MonadReader (Domain p t) m
        , T.Transport t
        ) => [p -> m i] -> [p -> m o] -> m Feature
mix inputs outputs = pure $ Feature Mix



instance Controller Mix
