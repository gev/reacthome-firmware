module Interface.GPIO.Input where

import Ivory.Language

class Input a where
    get :: a -> Ivory eff IBool
