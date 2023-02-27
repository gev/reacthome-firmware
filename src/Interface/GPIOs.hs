module Interface.GPIOs where

import           Core.Include
import           Core.Initialize
import           Ivory.Language
import           Ivory.Language.Module

class (Include a, Initialize a) => Inputs a where
    get   :: a -> Ix n -> Ivory eff IBool

class (Include a, Initialize a) => Outputs a where
    reset :: a -> Ix n -> Ivory eff ()
    set   :: a -> Ix n -> Ivory eff ()

уточнять
