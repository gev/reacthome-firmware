{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Buffer where

import           Core.Include
import           GHC.TypeNats
import           Ivory.Language


type Buffer n t = MemArea (Array n (Stored t))


buffer :: (IvoryZeroVal t, IvoryInit t, KnownNat n) => String -> Buffer n t
buffer id = area (id <> "_buffer") Nothing


instance (KnownNat n, IvoryType t) => Include (Buffer n t) where
    include = defMemArea
