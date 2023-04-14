{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Matrix
    ( Matrix
    , Matrix'
    , RunRows
    , matrix_
    , matrix
    , runRows_
    , runRows
    , runRowsFromList
    ) where

import           Control.Monad.Writer
import           Core.Context
import           Data.Area
import           GHC.TypeNats
import           Ivory.Language


type Matrix  n m t = Ref Global (Array  n (Array m (Stored t)))
type Matrix' n m t = MemArea    (Array  n (Array m (Stored t)))
type RunRows   m t = forall a.  (forall n. KnownNat n => Matrix' n m t -> a) -> a


matrix_ :: (MonadWriter Context w, IvoryZeroVal t, KnownNat n, KnownNat m)
        => String -> w (Matrix n m t)
matrix_ id = mkArea id Nothing


matrix :: (MonadWriter Context w, IvoryZeroVal t, IvoryInit t, KnownNat n, KnownNat m)
       => String -> [[t]] -> w (Matrix n m t)
matrix id v = mkArea id . Just . iarray $ (\r -> iarray $ ival <$> r) <$> v



runRows_ :: (IvoryInit t, IvoryZeroVal t, KnownNat m)
         => String -> Int -> RunRows m t
runRows_ id = run (area id Nothing)

runRows :: (IvoryInit t, IvoryZeroVal t, KnownNat m)
        => String -> [[t]] -> RunRows m t
runRows id xs = run (area id . Just . iarray $ (\r -> iarray $ ival <$> r) <$> xs) $ length xs

runRowsFromList :: (IvoryInit t, IvoryZeroVal t, KnownNat m)
                => String -> (c -> t) -> [[c]] -> RunRows m t
runRowsFromList id h xs = run (area id . Just . iarray $ (\r -> iarray $ ival . h <$> r) <$> xs) $ length xs



run :: forall m t. (forall n. KnownNat n => Matrix' n m t) -> Int -> RunRows m t
run v n f = go . someNatVal . fromIntegral $ n
    where go (SomeNat (p :: Proxy p)) = f (v :: Matrix' p m t)
