module Interface.Counter where

import           Ivory.Language
import           Ivory.Stdlib


class Counter t where
    readCounter :: t -> Ivory eff Uint32

    delay :: t -> Uint32 -> Ivory (ProcEffects s ()) ()
    delay t d =    do
        start <- readCounter t
        forever $ do
            current <- readCounter t
            when (current - start >=? d) breakOut
