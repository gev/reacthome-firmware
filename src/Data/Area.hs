{-# LANGUAGE FlexibleContexts #-}

module Data.Area where

import Control.Monad.State
import Core.Context
import Ivory.Language

mkArea ::
    (MonadState Context m, IvoryArea a, IvoryZero a) =>
    String ->
    Maybe (Init a) ->
    m (Ref Global a)
mkArea id v = do
    let a = area id v
    addArea a
    pure $ addrOf a
