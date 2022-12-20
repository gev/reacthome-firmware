module Interface.GPIO where

import           Include
import           Initialize
import           Ivory.Language
import           Ivory.Language.Module

class (Include a, Initialize a) => IN a where
  get :: a -> Ivory eff IBool

class (Include a, Initialize a) => OUT a where
  reset :: a -> Ivory eff ()
  set :: a -> Ivory eff ()

class (Include a, Initialize a) => I2C a

class (Include a, Initialize a) => SPI a
