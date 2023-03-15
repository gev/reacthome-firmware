{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}

module Interface.GPIOs.Outputs where

import           Core.Context
import           GHC.TypeNats
import           Interface.GPIO.Output
import           Ivory.Language
import           Ivory.Language.Module

class Include a => Outputs a where
    reset :: a -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
    set   :: a -> (forall n. KnownNat n => Ix n) -> Ivory eff ()

class (Output a, Outputs as) => MakeOutputs a as | a -> as where
    makeOutputs :: String -> [a] -> as
