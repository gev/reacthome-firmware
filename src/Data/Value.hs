{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Value
    ( Value
    , Values
    , RunValues
    , value_
    , value
    , values_
    , values
    , runValues_
    , runValues
    , runValuesFromList
    ) where

import           Core.Include
import           GHC.TypeNats
import           Ivory.Language



type Value  t   = MemArea (Stored t)
type Values n t = MemArea (Array n (Stored t))

type RunValues t = forall a. (forall n. KnownNat n => Values n t -> a) -> a



value_ :: (IvoryArea t, IvoryZero t) => String -> MemArea t
value_ id = area id Nothing

value :: (IvoryZeroVal t, IvoryInit t) => String -> t -> Value t
value id v = area id . Just $ ival v



values_ :: (IvoryArea t, IvoryZero t) => String -> MemArea t
values_ id = area id Nothing

values :: (KnownNat n, IvoryZeroVal t, IvoryInit t) => String -> [t] -> Values n t
values id v = area id . Just . iarray $ ival <$> v



runValues_ :: (IvoryInit t, IvoryZeroVal t)
           => String
           -> Int
           -> RunValues t
runValues_ id = run $ values_ id

runValues :: (IvoryInit t, IvoryZeroVal t)
          => String
          -> [t]
          -> RunValues t
runValues id xs = run (values id xs) $ length xs

runValuesFromList :: (IvoryInit t, IvoryZeroVal t)
                  => String
                  -> (c -> t)
                  -> [c]
                  -> RunValues t
runValuesFromList id h xs = run (values id $ h <$> xs) $ length xs



run :: forall a t. (forall n. KnownNat n => Values n t)
    -> Int
    -> (forall n. KnownNat n => Values n t -> a)
    -> a
run v n f = go . someNatVal . fromIntegral $ n
    where go (SomeNat (p :: Proxy p)) = f (v :: Values p t)



instance IvoryType t => Include (Value t) where
    include = defMemArea

instance (KnownNat n, IvoryType t) => Include (Values n t) where
    include = defMemArea
