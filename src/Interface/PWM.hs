module Interface.PWM where

import           Ivory.Language


data ModePMW
    = DUTY_HIGH
    | DUTY_LOW
    | FORCE_HIGH
    | FORCE_LOW


class PWM p where
    setDuty        :: p -> Uint16 -> Ivory eff ()
    resetCounter   :: p -> Ivory eff ()
    setMode        :: p -> ModePMW -> Ivory eff ()
