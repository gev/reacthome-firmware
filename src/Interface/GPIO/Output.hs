module Interface.GPIO.Output where

import           Core.Context
import           Ivory.Language
import           Ivory.Language.Module

class Include a => Output a where
    reset :: a -> Ivory eff ()
    set   :: a -> Ivory eff ()
