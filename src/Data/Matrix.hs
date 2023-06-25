{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Matrix
    ( Matrix
    , Matrix'
    , RunMatrix
    , matrix_
    , matrix
    , runMatrix_
    , runMatrix
    ) where

import           Control.Monad.Writer
import           Core.Context
import           Data.Area
import           GHC.TypeNats
import           Ivory.Language


type Matrix  n m t = Ref Global (Array  n (Array m (Stored t)))
type Matrix' n m t = MemArea    (Array  n (Array m (Stored t)))
type RunMatrix   t = forall a.  (forall n m. (KnownNat n, KnownNat m) => Matrix' n m t -> a) -> a


matrix_ :: (MonadWriter Context w, IvoryZeroVal t, KnownNat n, KnownNat m)
        => String -> w (Matrix n m t)
matrix_ id = mkArea id Nothing


matrix :: (MonadWriter Context w, IvoryZeroVal t, IvoryInit t, KnownNat n, KnownNat m)
       => String -> [[t]] -> w (Matrix n m t)
matrix id v = mkArea id . Just . iarray $ (\r -> iarray $ ival <$> r) <$> v




runMatrix_ :: (IvoryInit t, IvoryZeroVal t)
           => String -> Int -> Int -> RunMatrix t
runMatrix_ id = run' (area id Nothing)

runMatrix :: (IvoryInit t, IvoryZeroVal t)
          => String -> t -> Int -> Int -> RunMatrix t
runMatrix id v n m = run' (area id . Just . iarray . replicate n . iarray . replicate m . ival $ v) n m



run' :: forall t. (forall n m. (KnownNat n, KnownNat m) => Matrix' n m t) -> Int -> Int -> RunMatrix t
run' v n m f = go (someNatVal . fromIntegral $ n) (someNatVal . fromIntegral $ m)
    where go (SomeNat (p :: Proxy p)) (SomeNat (q :: Proxy q)) = f (v :: Matrix' p q t)
