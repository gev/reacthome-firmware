{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Fixed where

import           GHC.TypeNats



infixr 5 :>



data List n t where
  Nil  :: List 0 t
  (:>) :: t -> List n t -> List (n + 1) t



toList :: List n t -> [t]
toList Nil       = []
toList (x :> xs) = x : toList xs



instance Functor (List n) where
  fmap _ Nil       = Nil
  fmap f (x :> xs) = f x :> fmap f xs



instance Foldable (List n) where
  foldr _ z Nil       = z
  foldr f z (x :> xs) = f x (foldr f z xs)



instance Traversable (List n) where
  traverse _ Nil       = pure Nil
  traverse f (x :> xs) = (:>) <$> f x <*> traverse f xs



class MakeFrom n t f where
  from :: f -> List n t

instance MakeFrom 1 t t where
  from x1 =
    x1 :> Nil

instance MakeFrom 2 t (t, t) where
  from (x1, x2) =
    x1 :> from x2

instance MakeFrom 3 t (t, t, t) where
  from (x1, x2, x3) =
    x1 :> from (x2, x3)

instance MakeFrom 4 t (t, t, t, t) where
  from (x1, x2, x3, x4) =
    x1 :> from (x2, x3, x4)

instance MakeFrom 5 t (t, t, t, t, t) where
  from (x1, x2, x3, x4, x5) =
    x1 :> from (x2, x3, x4, x5)

instance MakeFrom 6 t (t, t, t, t, t, t) where
  from (x1, x2, x3, x4, x5, x6) =
    x1 :> from (x2, x3, x4, x5, x6)

instance MakeFrom 7 t (t, t, t, t, t, t, t) where
  from (x1, x2, x3, x4, x5, x6, x7) =
    x1 :> from (x2, x3, x4, x5, x6, x7)

instance MakeFrom 8 t (t, t, t, t, t, t, t, t) where
  from (x1, x2, x3, x4, x5, x6, x7, x8) =
    x1 :> from (x2, x3, x4, x5, x6, x7, x8)

instance MakeFrom 9 t (t, t, t, t, t, t, t, t, t) where
  from (x1, x2, x3, x4, x5, x6, x7, x8, x9) =
    x1 :> from (x2, x3, x4, x5, x6, x7, x8, x9)

instance MakeFrom 10 t (t, t, t, t, t, t, t, t, t, t) where
  from (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) =
    x1 :> from (x2, x3, x4, x5, x6, x7, x8, x9, x10)

instance MakeFrom 11 t (t, t, t, t, t, t, t, t, t, t, t) where
  from (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) =
    x1 :> from (x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)

instance MakeFrom 12 t (t, t, t, t, t, t, t, t, t, t, t, t) where
  from (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) =
    x1 :> from (x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)

instance MakeFrom 13 t (t, t, t, t, t, t, t, t, t, t, t, t, t) where
  from (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) =
    x1 :> from (x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)

instance MakeFrom 14 t (t, t, t, t, t, t, t, t, t, t, t, t, t, t) where
  from (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) =
    x1 :> from (x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)

instance MakeFrom 15 t (t, t, t, t, t, t, t, t, t, t, t, t, t, t, t) where
  from (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) =
    x1 :> from (x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)

instance MakeFrom 16 t (t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t) where
  from (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) =
    x1 :> from (x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16)
