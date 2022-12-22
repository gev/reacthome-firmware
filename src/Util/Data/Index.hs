{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}

module Util.Data.Index where

import           GHC.TypeNats
import           Include
import           Ivory.Language


type Index n = MemArea (Stored (Ix n))


index :: KnownNat n => String -> MemArea (Stored (Ix n))
index id = area (id <> "_index") $ Just $ ival (toIx (0 :: Sint32))


instance (KnownNat n) => Include (Index n) where
  include = defMemArea
