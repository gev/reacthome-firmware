{-# LANGUAGE FlexibleContexts #-}
module Data.Index where

import           Control.Monad.Writer
import           Core.Context
import           Data.Value
import           Ivory.Language


type Index t = Value t


index :: (MonadWriter Context m, IvoryZeroVal t, IvoryInit t, Num t)
      => String -> m (Index t)
index id = value (id <> "_index") 0
