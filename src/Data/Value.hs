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



value_ :: (Monad m, IvoryZeroVal t)
       => String -> WriterT Context m (Value t)
value_ id = mem id Nothing

value :: (Monad m, IvoryZeroVal t, IvoryInit t)
      => String -> t -> WriterT Context m (Value t)
value id v = mem id . Just $ ival v



values_ :: (Monad m, KnownNat n, IvoryZeroVal t)
        => String -> WriterT Context m (Values n t)
values_ id = mems id Nothing

values :: (Monad m, KnownNat n, IvoryZeroVal t, IvoryInit t)
        => String -> [t] -> WriterT Context m (Values n t)
values id v = mems id . Just . iarray $ ival <$> v



mem :: (Monad m, IvoryType t, IvoryZeroVal t)
    => String -> Maybe (Init (Stored t)) -> WriterT Context m (Value t)
mem id v = do
    let a = area id v
    include a
    pure a

mems :: (Monad m, KnownNat n, IvoryZeroVal t)
     => String -> Maybe (Init (Array n (Stored t))) -> WriterT Context m (Values n t)
mems id v = do
    let a = area id v
    include a
    pure a



runValues_ :: (IvoryInit t, IvoryZeroVal t)
           => String
           -> Int
           -> RunValues t
runValues_ id = run (area id Nothing)

runValues :: (Monad m, IvoryInit t, IvoryZeroVal t)
          => String
          -> [t]
          -> WriterT Context m (RunValues t)
runValues id xs =
    pure $ run (area id . Just . iarray $ ival <$> xs) $ length xs

runValuesFromList :: (Monad m, IvoryInit t, IvoryZeroVal t)
                  => String
                  -> (c -> t)
                  -> [c]
                  -> WriterT Context m (RunValues t)
runValuesFromList id h xs =
    pure $ run (area id . Just . iarray $ ival . h <$> xs) $ length xs



run :: forall t. (forall n. KnownNat n => Values n t)
    -> Int
    -> RunValues t
run v n f = do
        go . someNatVal . fromIntegral $ n
    where go (SomeNat (p :: Proxy p)) = f (v :: Values p t)



instance IvoryType t => Include (Value t) where
    include = include . defMemArea

instance (KnownNat n, IvoryType t) => Include (Values n t) where
    include = include . defMemArea
