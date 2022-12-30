{-# LANGUAGE DataKinds #-}

module Util.Data.Index where

import           GHC.TypeNats
import           Include
import           Ivory.Language
import           Util.Data.Class
import           Util.Data.Value


newtype Index n = Index { getIndex :: Value (Ix n) }


index :: KnownNat n => String -> Index n
index id = Index $ value (id <> "_index") 0


instance Include (Index n) where
    include = include . getIndex
