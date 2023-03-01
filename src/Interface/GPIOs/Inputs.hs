{-# LANGUAGE RankNTypes #-}

module Interface.GPIOs.Inputs where

import           Core.Include
import           Core.Initialize
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Module

class (Include a, Initialize a) => Inputs a where
    get   :: a -> (forall n. KnownNat n => Ix n) -> Ivory eff IBool
