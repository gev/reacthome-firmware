module Interface.Flash where

import Ivory.Language
import Prelude hiding (read)

data Flash p = Flash
    { read :: Uint32 -> forall eff. Ivory eff Uint32
    , write :: Uint32 -> forall eff. Uint32 -> Ivory eff ()
    , erase :: Uint32 -> forall eff. Ivory eff ()
    }

mkOffset :: Flash p -> Uint32 -> Flash p
mkOffset flash base =
    Flash
        { read = \offset -> read flash $ base + offset
        , write = \offset -> write flash $ base + offset
        , erase = \offset -> erase flash $ base + offset
        }
