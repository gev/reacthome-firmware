module Interface.GPIO.Input where

import           Ivory.Language
import           Ivory.Language.Module

data Pull = PullUp 
          | PullDown 
          | PullNone

class Input a where
    get     :: a -> Ivory eff IBool
    setPUPD :: a -> Pull -> Ivory eff ()