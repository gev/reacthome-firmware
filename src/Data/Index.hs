{-# LANGUAGE DataKinds #-}

module Data.Index where

import           Data.Value
import           Ivory.Language


type Index t = Value t


index :: (IvoryZeroVal t, IvoryInit t, Num t) => String -> Index t
index id = value (id <> "_index") 0
