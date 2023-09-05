module Interface.GPIO.OpenDrain where

import           Ivory.Language
import           Ivory.Language.Module

class OpenDrain a where
    reset :: a -> Ivory eff ()
    set   :: a -> Ivory eff ()
    get   :: a -> Ivory eff IBool
