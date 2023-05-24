module Interface.PWM where

import           Ivory.Language


data ModePMW
    = DUTY_LOW
    | DUTY_HIGH
    | FORCE_LOW
    | FORCE_HIGH


class PWM p where
    setDuty        :: p -> Uint16 -> Ivory eff ()
    resetCounter   :: p -> Ivory eff ()
    setMode        :: p -> ModePMW -> Ivory eff ()
