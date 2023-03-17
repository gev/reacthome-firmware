module Data.Buffer where

import           Control.Monad.Writer
import           Core.Context
import           Data.Value
import           GHC.TypeNats
import           Ivory.Language


type Buffer n t = Values n t


buffer :: (Monad m,  KnownNat n, IvoryZeroVal t)
       => String -> WriterT Context m (Buffer n t)
buffer id = values_ $ id <> "_buffer"
