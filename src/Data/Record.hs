{-# LANGUAGE DataKinds #-}


module Data.Record where

import           GHC.TypeNats
import           Ivory.Language


type Record t    = MemArea (Struct t)
type Records n t = MemArea (Array n (Struct t))



record_ :: IvoryStruct t => String -> Record t
record_ id = area id Nothing

record :: IvoryStruct t => String -> [InitStruct t] -> Record t
record id r = area id . Just $ istruct r



records_ :: (IvoryStruct t, KnownNat n) => String -> Records n t
records_ id = area id Nothing

records :: (IvoryStruct t, KnownNat n) => String -> [[InitStruct t]] -> Records n t
records id r = area id . Just . iarray $ istruct <$> r
