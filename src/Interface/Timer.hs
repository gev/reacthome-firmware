module Interface.Timer where

import           Interface
import           Interface.IRQ


class (Interface t, IRQ t) => Timer t
