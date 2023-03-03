{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}

module Interface.GPIOs.Inputs where

import           Core.Include
import           Core.Initialize
import           GHC.TypeNats
import           Interface.GPIO.Input
import           Ivory.Language
import           Ivory.Language.Module

class (Include a, Initialize a) => Inputs a where
    get :: a -> (forall n. KnownNat n => Ix n) -> Ivory eff IBool

class (Input a, Inputs as) => MakeInputs a as | a -> as where
    makeInputs :: String -> [a] -> as
