{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Value where

import           Core.Include
import           Ivory.Language


type Value t = MemArea (Stored t)


value :: (IvoryZeroVal t, IvoryInit t) => String -> t -> Value t
value id v = area id . Just $ ival v


instance IvoryType t => Include (Value t) where
    include = defMemArea
