module Interface.GPIO.Input where

import           Core.Include
import           Core.Initialize
import           Ivory.Language
import           Ivory.Language.Module

class Initialize a => Input a where
    get   :: a -> Ivory eff IBool
