{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Area where

import           Control.Monad.State
import           Core.Context
import           Ivory.Language


mkArea :: (MonadState Context m, IvoryArea area, IvoryZero area)
    => String -> Maybe (Init area) -> m (Ref Global area)
mkArea id v = do
    let a = area id v
    addArea a
    pure $ addrOf a
