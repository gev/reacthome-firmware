{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}

module Util.Buffer where

import           GHC.TypeNats
import           Include
import           Ivory.Language


type Buffer n t = MemArea (Array n (Stored t))


buffer :: (IvoryArea area, IvoryZero area) => String -> MemArea area
buffer id  = area (id <> "_buffer") Nothing


instance (KnownNat n, IvoryType t) => Include (Buffer n t) where
  include = defMemArea
