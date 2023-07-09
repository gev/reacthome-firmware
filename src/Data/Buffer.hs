{-# LANGUAGE FlexibleContexts #-}
module Data.Buffer where

import           Control.Monad.State
import           Core.Context
import           Data.Value
import           GHC.TypeNats
import           Ivory.Language


type Buffer n t = Values n t


buffer :: (MonadState Context m,  KnownNat n, IvoryZeroVal t)
       => String -> m (Buffer n t)
buffer id = values_ $ id <> "_buffer"
