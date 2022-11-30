module Interface where

import           Ivory.Language
import           Ivory.Language.Module

class Interface a where
  dependecies :: a -> [ModuleM ()]
  initialize :: a -> Ivory eff ()

