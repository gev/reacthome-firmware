module Interface.MCU where

import           Interface.Mac
import           Interface.SystemClock

class MCU mcu where
    mac         :: mcu -> Mac
    systemClock :: mcu -> SystemClock
