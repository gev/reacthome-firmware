module Interface.GPIO.Input where

import Ivory.Language
import Ivory.Language.Module

class Input a where
    get :: a -> Ivory eff IBool
