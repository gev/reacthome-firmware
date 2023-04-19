module Interface.PWM where

import Ivory.Language

class PWM p where
    configTimer :: p -> Ivory eff ()
    setDuty :: p -> Uint32 -> Ivory eff ()
