{-# LANGUAGE RankNTypes #-}

module Interface.GPIOs.Outputs where

import           Core.Include
import           Core.Initialize
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Module

class (Include a, Initialize a) => Outputs a where
    reset :: a -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
    set   :: a -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
