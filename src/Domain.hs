module Domain where

import           Interface.SystemClock (SystemClock)
import           Ivory.Language
import           Util.Data.Record
import           Util.Version

data Domain mcu = Domain
    { model   :: Uint8
    , version :: Record Version
    , clock   :: SystemClock
    , mcu     :: mcu
    }
