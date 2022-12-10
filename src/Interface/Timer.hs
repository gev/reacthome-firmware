module Interface.Timer where

import           Interface
import           Ivory.Language


class (Interface t) => Timer t where
  current :: t -> Uint32
  delay   :: t -> Uint32 -> Ivory eff ()
