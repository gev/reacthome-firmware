{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Data.Record
    ( Record
    , Records
    , record_
    , record
    , records_
    , records'
    , records
    ) where

import           Control.Monad.State
import           Core.Context
import           Data.Area
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Proxy



type Record    t = Ref Global (Struct t)
type Records n t = Ref Global (Array n (Struct t))


record_ :: (MonadState Context m, IvoryStruct t)
        => String -> m (Record t)
record_ id = mkArea id Nothing

record :: (MonadState Context m, IvoryStruct t)
       => String -> [InitStruct t] -> m (Record t)
record id r = mkArea id . Just $ istruct r



records_ :: (MonadState Context m, KnownNat n, IvoryStruct t)
         => String -> m (Records n t)
records_ id = mkArea id Nothing

records' :: forall m n t. (MonadState Context m, KnownNat n, IvoryStruct t)
         => String -> [InitStruct t] -> m (Records n t)
records' id r = do
    let n = fromIntegral $ natVal (aNat :: NatType n)
    records id $ replicate n r

records :: (MonadState Context m, KnownNat n, IvoryStruct t)
        => String -> [[InitStruct t]] -> m (Records n t)
records id r = mkArea id . Just . iarray $ istruct <$> r
