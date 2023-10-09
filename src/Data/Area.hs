{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Area where

import           Control.Monad.Writer
import           Core.Context
import           Ivory.Language


mkArea :: (MonadWriter Context m, IvoryArea area, IvoryZero area)
    => String -> Maybe (Init area) -> m (Ref Global area)
mkArea id v = do
    let a = area id v
    addArea a
    pure $ addrOf a