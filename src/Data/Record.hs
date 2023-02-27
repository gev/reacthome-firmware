{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Data.Record
    ( Record
    , Records
    , record_
    , record
    , records_
    , records
    , runRecords_
    , runRecords
    ) where

import           GHC.TypeNats
import           Ivory.Language


type Record t    = MemArea (Struct t)
type Records n t = MemArea (Array n (Struct t))

type RunRecords t a = IvoryStruct t
                   => (forall n. KnownNat n => Records n t -> a) -> a



record_ :: IvoryStruct t => String -> Record t
record_ id = area id Nothing

record :: IvoryStruct t => String -> [InitStruct t] -> Record t
record id r = area id . Just $ istruct r



records_ :: (IvoryStruct t, KnownNat n) => String -> Records n t
records_ id = area id Nothing

records :: (IvoryStruct t, KnownNat n) => String -> [[InitStruct t]] -> Records n t
records id r = area id . Just . iarray $ istruct <$> r



runRecords_ :: String
            -> Int
            -> RunRecords t a
runRecords_ name = run $ records_ name

runRecords :: String
           -> (c -> [InitStruct t])
           -> [c]
           -> RunRecords t a
runRecords name h xs = run (records name $ h <$> xs) $ length xs

run :: forall a t. (forall n. KnownNat n => Records n t)
    -> Int
    -> (forall n. KnownNat n => Records n t -> a)
    -> a
run r n f = go . someNatVal . fromIntegral $ n
    where go (SomeNat (p :: Proxy p)) = f (r :: Records p t)
