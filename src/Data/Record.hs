{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Data.Record
    ( Record
    , Records
    , Records'
    , RunRecords
    , record_
    , record
    , records_
    , records
    , runRecords_
    , runRecords
    , runRecordsFromList
    ) where

import           Control.Monad.Writer
import           Core.Context
import           Data.Area
import           GHC.TypeLits         (KnownSymbol)
import           GHC.TypeNats         (KnownNat, SomeNat (..), someNatVal)
import           Ivory.Language


type Record     t = Ref Global (Struct t)
type Records  n t = Ref Global (Array n (Struct t))
type Records' n t = MemArea    (Array n (Struct t))
type RunRecords t = forall a.  (forall n. KnownNat n => Records' n t -> a) -> a



record_ :: (MonadWriter Context m, IvoryStruct t)
        => String -> m (Record t)
record_ id = mkArea id Nothing

record :: (MonadWriter Context m, IvoryStruct t)
       => String -> [InitStruct t] -> m (Record t)
record id r = mkArea id . Just $ istruct r



records_ :: (MonadWriter Context m, KnownNat n, IvoryStruct t)
         => String -> m (Records n t)
records_ id = mkArea id Nothing

records :: (MonadWriter Context m, KnownNat n, IvoryStruct t)
        => String -> [[InitStruct t]] -> m (Records n t)
records id r = mkArea id . Just . iarray $ istruct <$> r



runRecords_ :: IvoryStruct t
             => String
             -> Int
             -> RunRecords t
runRecords_ id = run (area id Nothing)



runRecords :: IvoryStruct t
           => String
           -> [[InitStruct t]]
           -> RunRecords t
runRecords id xs = run (area id . Just . iarray $ istruct <$> xs) $ length xs

runRecordsFromList :: IvoryStruct t
                  => String
                  -> (c -> [InitStruct t])
                  -> [c]
                  -> RunRecords t
runRecordsFromList id h xs = run (area id . Just . iarray $ istruct . h <$> xs) $ length xs



run :: forall t. (forall n. KnownNat n => Records' n t)
    -> Int
    -> RunRecords t
run r n f = go . someNatVal . fromIntegral $ n
    where go (SomeNat (p :: Proxy p)) = f (r :: Records' p t)
