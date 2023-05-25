module Interface.PWM where

import           Interface.Timer
import           Ivory.Language


data ModePMW
    = HIGH
    | LOW
    | FORCE_HIGH
    | FORCE_LOW


class Timer p => PWM p where
    setMode       :: p -> ModePMW -> Ivory eff ()
    setDuty       :: p -> Uint16 -> Ivory eff ()
    resetCounter  :: p -> Ivory eff ()
