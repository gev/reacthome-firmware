module Interface.GPIO where

import           Core.Include
import           Core.Initialize
import           Ivory.Language
import           Ivory.Language.Module

class (Include a, Initialize a) => In  a where
    get   :: a -> Ivory eff IBool

class (Include a, Initialize a) => Out a where
    reset :: a -> Ivory eff ()
    set   :: a -> Ivory eff ()

class (Include a, Initialize a) => I2C a

class (Include a, Initialize a) => SPI a
