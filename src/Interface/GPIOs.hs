module Interface.GPIOs where

import           Ivory.Language
import           Ivory.Language.Module

class Inputs  a where
    get   :: a -> Ivory eff IBool

class Outputs a where
    reset :: a -> Uint8 -> Ivory eff ()
    set   :: a -> Uint8 -> Ivory eff ()
