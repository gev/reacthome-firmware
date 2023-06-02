module Interface.Counter where

import           Ivory.Language
import           Ivory.Stdlib


class Counter t where
    readCounter :: t -> Ivory eff Uint16

    usDelay :: t -> Uint16 -> Ivory (ProcEffects s ()) ()
    usDelay t d =    do
        start <- readCounter t
        forever $ do
            current <- readCounter t
            when (current - start >=? d) breakOut
