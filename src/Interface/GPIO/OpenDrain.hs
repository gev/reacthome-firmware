module Interface.GPIO.OpenDrain
    ( Input
    , Output
    , OpenDrain
    , get
    , reset
    , set
    ) where

import           Interface.GPIO.Output
import           Ivory.Language
import           Ivory.Language.Module

class Output a => OpenDrain a
