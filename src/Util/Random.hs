{-# LANGUAGE FlexibleContexts #-}

module Util.Random where

import           Control.Monad.State (MonadState)
import           Core.Context
import           Data.Value
import           Ivory.Language



data Random t = Random
    { x :: Value t
    , y :: Value t
    , z :: Value t
    , a :: Value t
    }



mkRandom :: (MonadState Context m, IvoryZeroVal t, IvoryInit t, Num t)
         => String -> t -> m (Random t)
mkRandom id seed = do
    let name = "random_" <> id
    x <- value (name <> "_x") 0
    y <- value (name <> "_y") 0
    z <- value (name <> "_z") 0
    a <- value (name <> "_a") seed
    pure $ Random x y z a



type RandomUin8  = Random Uint8
type RandomUin16 = Random Uint16
type RandomUin32 = Random Uint32



class (IvoryBits t, IvoryStore t) => Randomize t where
    shift :: t

    next :: Random t -> Ivory eff t
    next (Random x y z a) = do
        x' <- deref x
        y' <- deref y
        z' <- deref z
        a' <- deref a
        let t' = x' .^ (x' `iShiftL` shift)
        store x y'
        store y z'
        store z a'
        let r' = z' .^ t' .^ (z' `iShiftR` 1) .^ (t' `iShiftL` 1)
        store a r'
        pure r'



instance Randomize Uint8 where
    shift = 4



instance Randomize Uint16 where
    shift = 8



instance Randomize Uint32 where
    shift = 16
