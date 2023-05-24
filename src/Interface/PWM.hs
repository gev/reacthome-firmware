module Interface.PWM where

import           Ivory.Language

type Prescaler = Uint32
type Period = Uint32

data ModePMW 
    = FORCE_LOW
    | FORCE_HIGH
    | DUTY_LOW
    | DURY_HIGH


class PWM p where
    setDuty        :: p -> Uint16 -> Ivory eff ()
    resetCounter   :: p -> Ivory eff ()
    setModePWM     :: p -> ModePMW -> Ivory eff ()
