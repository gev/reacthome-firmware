{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Arrays
    ( Value
    , Values
    , Values'
    , RunValues
    , value_
    , value
    , values_
    , values
    , runValues_
    , runValues
    , runValuesFromList
    ) where

import           Control.Monad.Writer
import           Core.Context
import           Data.Area
import           GHC.TypeNats
import           Ivory.Language



type Arrays'  n m t = MemArea    (Array  n (Stored t))
type RunArrays    t = forall a.  (forall n. KnownNat n => Arrays n m t -> a) -> a



value_ :: (MonadWriter Context m, IvoryZeroVal t)
       => String -> m (Value t)
value_ id = mkArea id Nothing

value :: (MonadWriter Context m, IvoryZeroVal t, IvoryInit t)
      => String -> t -> m (Value t)
value id v = mkArea id . Just $ ival v



values_ :: (MonadWriter Context m, KnownNat n, IvoryZeroVal t)
        => String -> m (Values n t)
values_ id = mkArea id Nothing

values :: (MonadWriter Context m, KnownNat n, IvoryZeroVal t, IvoryInit t)
       => String -> [t] -> m (Values n t)
values id v = mkArea id . Just . iarray $ ival <$> v



runValues_ :: (IvoryInit t, IvoryZeroVal t)
           => String -> Int -> RunValues t
runValues_ id = run (area id Nothing)

runValues :: (IvoryInit t, IvoryZeroVal t)
          => String -> [t] -> RunValues t
runValues id xs = run (area id . Just . iarray $ ival <$> xs) $ length xs

runValuesFromList :: (IvoryInit t, IvoryZeroVal t)
                  => String -> (c -> t) -> [c] -> RunValues t
runValuesFromList id h xs = run (area id . Just . iarray $ ival . h <$> xs) $ length xs



run :: forall t. (forall n. KnownNat n => Values' n t) -> Int -> RunValues t
run v n f = go . someNatVal . fromIntegral $ n
    where go (SomeNat (p :: Proxy p)) = f (v :: Values' p t)
