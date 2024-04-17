module Interface.ADC where

import           Ivory.Language

class ADC a where
    getResolution  :: a -> Uint16

    getAnalog      :: a -> Ivory eff Uint16

    getReduced     :: a -> Ivory eff IFloat
    getReduced a = (/ (2 ** safeCast (getResolution a))) . safeCast <$> getAnalog a
