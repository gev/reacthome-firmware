{-# LANGUAGE DataKinds #-}


module Data.Record where

import           Ivory.Language


type Record t = MemArea (Struct t)


record :: IvoryStruct t => String -> [InitStruct t] -> Record t
record id r = area id $ Just (istruct r)
