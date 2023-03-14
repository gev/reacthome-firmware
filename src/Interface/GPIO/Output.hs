module Interface.GPIO.Output where

import           Core.Include
import           Core.Initialize
import           Ivory.Language
import           Ivory.Language.Module

class Initialize a => Output a where
    reset :: a -> Ivory eff ()
    set   :: a -> Ivory eff ()
