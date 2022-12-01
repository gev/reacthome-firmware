module Interface.Timer where

import           Interface
import           Ivory.Language
import           Ivory.Language.Module


class Interface a => Timer a where
    setFrequency :: a -> Uint32 -> Ivory eff ()
    enable       :: a -> Ivory eff ()
