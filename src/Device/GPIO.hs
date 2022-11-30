module Device.GPIO where

import           Ivory.Language
import           Ivory.Language.Module

class GPIO a where
  dependecies :: a -> [ModuleM ()]
  initialize :: a -> Ivory eff ()

class GPIO a => IN a where
  getBit :: a -> Ivory eff IBool

class GPIO a => OUT a where
  reset :: a -> Ivory eff ()
  set :: a -> Ivory eff ()

class GPIO a => USART a

class GPIO a => I2C a

class GPIO a => SPI a
