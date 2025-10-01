module Interface.Timer where

import Core.Handler
import Ivory.Language

data HandleTimer t = HandleTimer
    { timer :: t
    , handle :: forall eff. Ivory eff ()
    }

class (Handler HandleTimer t) => Timer t where
    setCounter :: t -> Uint32 -> Ivory eff ()
    getCounter :: t -> Ivory eff Uint32
    enableInterrupt :: t -> Ivory eff ()
    disableInterrupt :: t -> Ivory eff ()

    resetCounter :: t -> Ivory eff ()
    resetCounter t = setCounter t 0
