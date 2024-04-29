{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module Data.Fixed where

import           Control.Monad       (liftM2, void)
import           Data.Proxy
import           GHC.TypeNats
import           Language.Haskell.TH
import           Prelude             hiding (fromEnum, toEnum, zipWith)



infixr 5 :>
data List n t where
  Nil  :: List 0 t
  (:>) :: t -> List (n - 1) t -> List n t



deriving instance Functor     (List n)
deriving instance Foldable    (List n)
deriving instance Traversable (List n)



toList :: List n t -> [t]
toList Nil       = []
toList (x :> xs) = x : toList xs


{-
  TODO: Unsafe!!!! Only infinite lists!
-}
fromList :: [t] -> List n t
fromList (x : xs) = x :> fromList xs



head :: List n t -> t
head (x :> _) = x

tail :: List n t -> List (n - 1) t
tail (_ :> xs) = xs



zipWith :: (t -> u -> v) -> List n t -> List n u -> List n v
zipWith _   Nil  _            = Nil
zipWith _    _  Nil           = Nil
zipWith f (x :> xs) (y :> ys) = f x y :> zipWith f xs ys

zipWithM :: Monad m => (t -> u -> m v) -> List n t -> List n u -> m (List n v)
zipWithM _   Nil  _            = pure Nil
zipWithM _    _  Nil           = pure Nil
zipWithM f (x :> xs) (y :> ys) = liftM2 (:>) (f x y) (zipWithM f xs ys)

zipWithM_ :: Monad m => (t -> u -> m v) -> List n t -> List n u -> m ()
zipWithM_ f xs = void . zipWithM f xs



fromEnum :: Integral i => i -> List n i
fromEnum n = n :> fromEnum (n + 1)

ints :: Integral i => List n i
ints = fromEnum 0

nats :: Integral i => List n i
nats = fromEnum 1

class MakeFrom n f t | f -> n, f -> t where
  from :: f -> List n t



flip foldMap [2 .. 64] $ \i -> do
  let
    -- type part
    t = varT $ mkName "t"
    tupleTyCon = conT $ tupleTypeName i
    tupleType = foldl appT tupleTyCon (replicate i t)
    intLit = litT $ numTyLit $ fromIntegral i
    -- body part
    vars = map (\x -> mkName ("x" <> show x)) [1 .. i]
    nil = conE 'Nil
    cons x y = conE '(:>) `appE` x `appE` y
    list = foldr (step . varE) nil vars
    step head_ tail_ = head_ `cons` tail_
  [d|
    instance MakeFrom $intLit $tupleType $t where
      from $(tupP $ map varP vars) = $(list)
    |]
