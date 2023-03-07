{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Data.Record
    ( Record
    , Records
    , RunRecords
    , record_
    , record
    , records_
    , records
    , runRecords_
    , runRecords
    , runRecordsFromList
    , get
    , set
    ) where

import           GHC.TypeLits   (KnownSymbol)
import           GHC.TypeNats   (KnownNat, SomeNat (..), someNatVal)
import           Ivory.Language


type Record t    = MemArea (Struct t)
type Records n t = MemArea (Array n (Struct t))

type RunRecords t = forall a. (forall n. KnownNat n => Records n t -> a) -> a



record_ :: IvoryStruct t => String -> Record t
record_ id = area id Nothing

record :: IvoryStruct t => String -> [InitStruct t] -> Record t
record id r = area id . Just $ istruct r



records_ :: (IvoryStruct t, KnownNat n) => String -> Records n t
records_ id = area id Nothing

records :: (IvoryStruct t, KnownNat n) => String -> [[InitStruct t]] -> Records n t
records id r = area id . Just . iarray $ istruct <$> r



runRecords_ :: IvoryStruct t
            => String
            -> Int
            -> RunRecords t
runRecords_ name = run $ records_ name

runRecords :: IvoryStruct t
           => String
           -> [[InitStruct t]]
           -> RunRecords t
runRecords name xs = run (records name xs) $ length xs

runRecordsFromList :: IvoryStruct t
                  => String
                  -> (c -> [InitStruct t])
                  -> [c]
                  -> RunRecords t
runRecordsFromList name h xs = run (records name $ h <$> xs) $ length xs



run :: forall a t. (forall n. KnownNat n => Records n t)
    -> Int
    -> (forall n. KnownNat n => Records n t -> a)
    -> a
run r n f = go . someNatVal . fromIntegral $ n
    where go (SomeNat (p :: Proxy p)) = f (r :: Records p t)



get :: (IvoryStore t, KnownNat n, KnownSymbol r, IvoryStruct r)
    => Label r (Stored t)
    -> Records n r
    -> Ix n
    -> Ivory eff t
get f rs i = deref $ addrOf rs ! i ~> f

set :: (IvoryStore t, KnownNat n, IvoryStruct r)
    => Label r (Stored t)
    -> Records n r
    -> Ix n
    -> t
    -> (forall eff. Ivory eff ())
set f rs i = store $ addrOf rs ! i ~> f
