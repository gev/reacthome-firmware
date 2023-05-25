module Interface.PWM where

import           Ivory.Language


data ModePMW
    = HIGH
    | LOW
    | FORCE_HIGH
    | FORCE_LOW


class PWM p where
    setDuty        :: p -> Uint16 -> Ivory eff ()
    resetCounter   :: p -> Ivory eff ()
    setMode        :: p -> ModePMW -> Ivory eff ()
