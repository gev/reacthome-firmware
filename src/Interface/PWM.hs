module Interface.PWM where

import Ivory.Language

type Prescaller = Uint32
type Period = Uint32

class PWM p where
    configTimer :: p -> Prescaller -> Period -> Ivory eff ()
    setDuty :: p -> Uint32 -> Ivory eff ()
