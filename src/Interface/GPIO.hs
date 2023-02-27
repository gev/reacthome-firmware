module Interface.GPIO where

import           Core.Include
import           Core.Initialize
import           Ivory.Language
import           Ivory.Language.Module

class (Include a, Initialize a) => Input  a where
    get   :: a -> Ivory eff IBool

class (Include a, Initialize a) => Output a where
    reset :: a -> Ivory eff ()
    set   :: a -> Ivory eff ()
