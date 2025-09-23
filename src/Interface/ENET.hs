module Interface.ENET where

import Core.Handler
import Ivory.Language

data HandleEnet e = HandleEnet
    { enet :: e
    , handle :: forall s. Ivory (ProcEffects s ()) ()
    }

class (Handler HandleEnet e) => Enet e where
    rxFrameSize :: e -> Ivory eff Uint32
