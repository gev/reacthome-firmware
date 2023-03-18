{-# LANGUAGE FlexibleContexts #-}
module Data.Buffer where

import           Control.Monad.Writer
import           Core.Context
import           Data.Value
import           GHC.TypeNats
import           Ivory.Language


type Buffer n t = Values n t


buffer :: (MonadWriter Context m,  KnownNat n, IvoryZeroVal t)
       => String -> m (Buffer n t)
buffer id = values_ $ id <> "_buffer"
