{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImpredicativeTypes  #-}
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

import           Control.Monad.Writer
import           Core.Context
import           GHC.TypeNats
import           Ivory.Language



type Value     t = MemArea   (Stored t)
type Values  n t = MemArea   (Array  n (Stored t))
type RunValues t = forall a. (forall n. KnownNat n => Values n t -> a) -> a



mem :: (Monad m, IvoryArea a, IvoryZero a)
    => String -> Maybe (Init a) -> WriterT Context m (MemArea a)
mem id v = do
    let a = area id v
    include $ defMemArea a
    pure a



value_ :: (Monad m, IvoryZeroVal t)
       => String -> WriterT Context m (Value t)
value_ id = mem id Nothing

value :: (Monad m, IvoryZeroVal t, IvoryInit t)
      => String -> t -> WriterT Context m (Value t)
value id v = mem id . Just $ ival v



values_ :: (Monad m, KnownNat n, IvoryZeroVal t)
        => String -> WriterT Context m (Values n t)
values_ id = mem id Nothing

values :: (Monad m, KnownNat n, IvoryZeroVal t, IvoryInit t)
        => String -> [t] -> WriterT Context m (Values n t)
values id v = mem id . Just . iarray $ ival <$> v




runValues_ :: (Monad m, IvoryZeroVal t)
           => String
           -> Int
           -> (forall n. KnownNat n => Values n t -> RunValues t)
           -> WriterT Context m (RunValues t)
runValues_ id = run $ values_ id

runValues :: (Monad m, IvoryInit t, IvoryZeroVal t)
          => String
          -> [t]
          -> (forall n. KnownNat n => Values n t -> RunValues t)
          -> WriterT Context m (RunValues t)
runValues id xs = run (values id xs) $ length xs

runValuesFromList :: (Monad m, IvoryInit t, IvoryZeroVal t)
                  => String
                  -> (c -> t)
                  -> [c]
                  -> (forall n. KnownNat n => Values n t -> RunValues t)
                  -> WriterT Context m (RunValues t)
runValuesFromList id h xs = run (values id $ h <$> xs) $ length xs



run :: forall a m t. Monad m
    => (forall n. KnownNat n => WriterT Context m (Values n t))
    -> Int
    -> (forall n. KnownNat n => Values n t -> a)
    -> WriterT Context m a
run v n f = do
    let go (SomeNat (p :: Proxy p)) = do
            v' <- v
            pure $ f (v' :: Values p t)
    go . someNatVal . fromIntegral $ n
