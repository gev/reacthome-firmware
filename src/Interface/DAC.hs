module Interface.DAC where

import           Ivory.Language

class DAC a where
    getResolution  :: a -> Uint16

    getRefVoltage  :: a -> IFloat

    setAnalog      :: a -> Uint16 -> Ivory eff ()

    setReduced     :: a -> IFloat -> Ivory eff ()
    setReduced  a v = setAnalog a . castDefault $ v * ((2 ** safeCast (getResolution a)) - 1)


