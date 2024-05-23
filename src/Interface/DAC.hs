module Interface.DAC where

import           Ivory.Language

class DAC a where
    getResolution  :: a -> Uint16

    getRefVoltage  :: a -> IFloat

    setAnalog      :: a -> Uint16 -> Ivory eff ()

    setVoltage     :: a -> IFloat -> Ivory eff ()
    setVoltage  a v = setAnalog a . castDefault $ (v / (getRefVoltage a / (2 ** safeCast (getResolution a))))


