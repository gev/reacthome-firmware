{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Value
    ( Value
    , Values
    , value_
    , value
    , values_
    , values
    , runValues_
    , runValues
    ) where

import           Core.Include
import           GHC.TypeNats
import           Ivory.Language



type Value  t   = MemArea (Stored t)
type Values n t = MemArea (Array n (Stored t))

type RunValues t a = (IvoryInit t, IvoryZeroVal t)
                  => (forall n. KnownNat n => Values n t -> a) -> a



value_ :: (IvoryArea t, IvoryZero t) => String -> MemArea t
value_ id = area id Nothing

value :: (IvoryZeroVal t, IvoryInit t) => String -> t -> Value t
value id v = area id . Just $ ival v



values_ :: (IvoryArea t, IvoryZero t) => String -> MemArea t
values_ id = area id Nothing

values :: (KnownNat n, IvoryZeroVal t, IvoryInit t) => String -> [t] -> Values n t
values id v = area id . Just . iarray $ ival <$> v



runValues_ :: String
           -> Int
           -> RunValues t a
runValues_ id = run $ values_ id

runValues :: String
          -> (c -> t)
          -> [c]
          -> RunValues t a
runValues id h xs = run (values id $ h <$> xs) $ length xs

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
