module Interface.Timer where

import           Interface
import           Interface.IRQ


class (Interface t) => Timer t
