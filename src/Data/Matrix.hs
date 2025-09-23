{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Matrix (
    Matrix,
    matrix_,
    matrix',
    matrix,
) where

import Control.Monad.State
import Core.Context
import Data.Area
import GHC.TypeNats
import Ivory.Language
import Ivory.Language.Proxy

type Matrix n m t = Ref Global (Array n (Array m (Stored t)))

matrix_ ::
    ( MonadState Context w
    , IvoryZeroVal t
    , KnownNat n
    , KnownNat m
    ) =>
    String ->
    w (Matrix n m t)
matrix_ id = mkArea id Nothing

matrix' ::
    forall m n t w.
    ( KnownNat n
    , KnownNat m
    , MonadState Context w
    , IvoryZeroVal t
    , IvoryInit t
    ) =>
    String ->
    t ->
    w (Matrix n m t)
matrix' id v = do
    let n' = fromTypeNat (aNat :: NatType n)
    let m' = fromTypeNat (aNat :: NatType m)
    matrix id . fill n' . fill m' $ v
  where
    fill = replicate . fromIntegral

matrix ::
    ( MonadState Context w
    , IvoryZeroVal t
    , IvoryInit t
    , KnownNat n
    , KnownNat m
    ) =>
    String ->
    [[t]] ->
    w (Matrix n m t)
matrix id v = mkArea id . Just . iarray $ (\r -> iarray $ ival <$> r) <$> v
