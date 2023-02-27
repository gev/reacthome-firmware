module Interface.GPIO where

import           Ivory.Language
import           Ivory.Language.Module

class Input  a where
    get   :: a -> Ivory eff IBool

class Output a where
    reset :: a -> Ivory eff ()
    set   :: a -> Ivory eff ()
