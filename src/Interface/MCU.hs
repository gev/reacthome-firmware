module Interface.MCU where

import           Core.Include
import           Interface.Mac
import           Interface.SystemClock

class Include mcu => MCU mcu where
    model       :: mcu -> String
    hasFPU      :: mcu -> Bool
    mac         :: mcu -> String -> Mac
    systemClock :: mcu -> SystemClock
