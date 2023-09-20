module Interface.GPIO.Output
    ( Input
    , Output
    , get
    , reset
    , set
    ) where

import           Interface.GPIO.Input
import           Ivory.Language
import           Ivory.Language.Module

class Input a => Output a where
    reset :: a -> Ivory eff ()
    set   :: a -> Ivory eff ()
