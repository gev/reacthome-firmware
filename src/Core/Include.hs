module Core.Include where

import           Ivory.Language
import           Ivory.Language.Module


class Include a where
    include :: a -> ModuleM ()
