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

import           Data.Proxy
import           GHC.TypeNats
import           Language.Haskell.TH

infixr 5 :>
data List n t where
  Nil  :: List 0 t
  (:>) :: t -> List n t -> List (n + 1) t

deriving instance Functor     (List n)
deriving instance Foldable    (List n)
deriving instance Traversable (List n)

toList :: List n t -> [t]
toList Nil       = []
toList (x :> xs) = x : toList xs

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
