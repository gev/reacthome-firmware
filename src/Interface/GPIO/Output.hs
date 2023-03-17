module Interface.GPIO.Output where

import           Ivory.Language
import           Ivory.Language.Module

class Output a where
    reset :: a -> Ivory eff ()
    set   :: a -> Ivory eff ()
