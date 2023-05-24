module Interface.PWM where

import           Ivory.Language

type Prescaler = Uint32
type Period = Uint32

class PWM p where
    setDuty        :: p -> Uint16 -> Ivory eff ()
    resetCounter   :: p -> Ivory eff ()
