module Interface.GPIO where

import           Interface
import           Ivory.Language
import           Ivory.Language.Module

class Interface a => IN a where
  get :: a -> Ivory eff IBool

class Interface a => OUT a where
  reset :: a -> Ivory eff ()
  set :: a -> Ivory eff ()

class Interface a => I2C a

class Interface a => SPI a
