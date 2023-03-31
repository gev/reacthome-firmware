{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Matrix
    ( Matrix
    , matrix_
    , matrix
    ) where

import           Control.Monad.Writer
import           Core.Context
import           Data.Area
import           GHC.TypeNats
import           Ivory.Language


type Matrix n m t = Ref Global (Array  n (Array m (Stored t)))


matrix_ :: (MonadWriter Context w, IvoryZeroVal t, KnownNat n, KnownNat m)
        => String -> w (Matrix n m t)
matrix_ id = mkArea id Nothing


matrix :: (MonadWriter Context w, IvoryZeroVal t, IvoryInit t, KnownNat n, KnownNat m)
       => String -> [[t]] -> w (Matrix n m t)
matrix id v = mkArea id . Just . iarray $ (\r -> iarray $ ival <$> r) <$> v
