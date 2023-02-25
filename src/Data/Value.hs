{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Value where

import           Core.Include
import           GHC.TypeNats
import           Ivory.Language



type Value  t   = MemArea (Stored t)
type Values n t = MemArea (Array n (Stored t))



value_ :: (IvoryArea t, IvoryZero t) => String -> MemArea t
value_ id = area id Nothing

value :: (IvoryZeroVal t, IvoryInit t) => String -> t -> Value t
value id v = area id . Just $ ival v



values_ :: (IvoryArea t, IvoryZero t) => String -> MemArea t
values_ id = area id Nothing

values :: (KnownNat n, IvoryZeroVal t, IvoryInit t) => String -> [t] -> Values n t
values id v = area id . Just . iarray $ ival <$> v



instance IvoryType t => Include (Value t) where
    include = defMemArea

instance (KnownNat n, IvoryType t) => Include (Values n t) where
    include = defMemArea
