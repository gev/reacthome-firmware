module Interface.Timer where

import           Interface
import           Ivory.Language
import           Ivory.Stdlib


class (Interface t) => Timer t where
  readCounter :: t -> Ivory eff Uint32

  delay :: t -> Uint32 -> Ivory (ProcEffects s ()) ()
  delay t d =  do
    start <- readCounter t
    forever $ do
      current <-  readCounter t
      when (current - start >=? d) breakOut
