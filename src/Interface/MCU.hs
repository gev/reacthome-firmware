module Interface.MCU where

import           Core.Include
import           Interface.Mac
import           Interface.SystemClock

class Include mcu => MCU mcu where
    mac         :: mcu -> String -> Mac
    systemClock :: mcu -> SystemClock
