{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Index where

import           Core.Include
import           Data.Class
import           Data.Value
import           GHC.TypeNats
import           Ivory.Language


newtype Index t = Index { getIndex :: Value t }


index :: (IvoryZeroVal t, IvoryInit t, Num t) => String -> Index t
index id = Index $ value (id <> "_index") 0


instance Include (Index t)  where
    include = include . getIndex

instance IvoryStore t => Val Index t where
  setValue = setValue . getIndex
  getValue = getValue . getIndex
