module Interface.GPIO.Input where

import           Core.Context
import           Ivory.Language
import           Ivory.Language.Module

class Include a => Input a where
    get   :: a -> Ivory eff IBool
