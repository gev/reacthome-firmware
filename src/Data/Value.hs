{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Value (
    Value,
    Values,
    value_,
    value,
    values_,
    values',
    values,
) where

import Control.Monad.State
import Core.Context
import Data.Area
import GHC.TypeNats
import Ivory.Language
import Ivory.Language.Proxy

type Value t = Ref Global (Stored t)
type Values n t = Ref Global (Array n (Stored t))

value_ ::
    (MonadState Context m, IvoryZeroVal t) =>
    String ->
    m (Value t)
value_ id = mkArea id Nothing

value ::
    (MonadState Context m, IvoryZeroVal t, IvoryInit t) =>
    String ->
    t ->
    m (Value t)
value id v = mkArea id . Just $ ival v

values_ ::
    (MonadState Context m, KnownNat n, IvoryZeroVal t) =>
    String ->
    m (Values n t)
values_ id = mkArea id Nothing

values' ::
    forall m n t.
    (MonadState Context m, KnownNat n, IvoryZeroVal t, IvoryInit t) =>
    String ->
    t ->
    m (Values n t)
values' id v = do
    let n = fromIntegral $ natVal (aNat :: NatType n)
    values id $ replicate n v

values ::
    (MonadState Context m, KnownNat n, IvoryZeroVal t, IvoryInit t) =>
    String ->
    [t] ->
    m (Values n t)
values id v = mkArea id . Just . iarray $ ival <$> v
