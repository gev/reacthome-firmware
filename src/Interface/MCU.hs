module Interface.MCU where

import           Interface.Mac
import           Interface.SystemClock

class MCU mcu where
    mac         :: mcu -> String -> Mac'
    systemClock :: mcu -> SystemClock
