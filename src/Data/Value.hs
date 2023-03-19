{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Value
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
import           GHC.TypeNats
import           Ivory.Language



type Value     t = Ref Global (Stored t)
type Values  n t = Ref Global (Array  n (Stored t))
type Values' n t = MemArea    (Array  n (Stored t))
type RunValues t = forall a.  (forall n. KnownNat n => Values' n t -> a) -> a



value_ :: (MonadWriter Context m, IvoryZeroVal t)
       => String -> m (Value t)
value_ id = mem id Nothing

value :: (MonadWriter Context m, IvoryZeroVal t, IvoryInit t)
      => String -> t -> m (Value t)
value id v = mem id . Just $ ival v



values_ :: (MonadWriter Context m, KnownNat n, IvoryZeroVal t)
        => String -> m (Values n t)
values_ id = mem id Nothing

values :: (MonadWriter Context m, KnownNat n, IvoryZeroVal t, IvoryInit t)
       => String -> [t] -> m (Values n t)
values id v = mem id . Just . iarray $ ival <$> v




mem :: (MonadWriter Context m, IvoryArea area, IvoryZero area)
    => String -> Maybe (Init area) -> m (Ref 'Global area)
mem id v = do
    let a = area id v
    addArea a
    pure $ addrOf a



runValues_ :: (IvoryInit t, IvoryZeroVal t)
           => String -> Int -> RunValues t
runValues_ id = run (area id Nothing)

runValues :: (MonadWriter Context m, IvoryInit t, IvoryZeroVal t)
          => String -> [t] -> m (RunValues t)
runValues id xs =
    pure $ run (area id . Just . iarray $ ival <$> xs) $ length xs

runValuesFromList :: (MonadWriter Context m, IvoryInit t, IvoryZeroVal t)
                  => String -> (c -> t) -> [c] -> m (RunValues t)
runValuesFromList id h xs =
    pure $ run (area id . Just . iarray $ ival . h <$> xs) $ length xs



run :: forall t. (forall n. KnownNat n => Values' n t)
    -> Int
    -> RunValues t
run v n f = do
        go . someNatVal . fromIntegral $ n
    where go (SomeNat (p :: Proxy p)) = f (v :: Values' p t)
