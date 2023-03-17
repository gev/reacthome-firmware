module Data.Index where

import           Control.Monad.Writer
import           Core.Context
import           Data.Value
import           Ivory.Language


type Index t = Value t


index :: (Monad m, IvoryZeroVal t, IvoryInit t, Num t)
      => String -> WriterT Context m (Index t)
index id = value (id <> "_index") 0
