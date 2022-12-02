module Interface where

import           Ivory.Language
import           Ivory.Language.Module

class Interface a where
  dependencies  :: a -> [ModuleM ()]
  initialize    :: a -> Ivory eff ()
