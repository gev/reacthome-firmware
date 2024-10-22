module Interface.ADC where

import           Ivory.Language

class ADC a where
    getResolution  :: a -> Uint16

    getAnalog      :: a -> Ivory (ProcEffects s t) Uint16

    getReduced     :: a -> Ivory (ProcEffects s t) IFloat

    getReduced a = (/ ((2 ** safeCast (getResolution a)) - 1)) . safeCast <$> getAnalog a
