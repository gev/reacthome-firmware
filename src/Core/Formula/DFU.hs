module Core.Formula.DFU where

import Control.Monad.Reader
import Control.Monad.State
import Core.Context
import Core.Controller
import Core.Domain
import Interface.MCU
import Ivory.Language

data DFU p = forall i1 i2 t. (Controller i1) => DFU
    { name :: String
    , model :: Uint8
    , version :: (Int, Int)
    , shouldInit :: IBool
    , mcu :: MCU p
    , quartzFrequency :: Int
    , systemFrequency :: Int
    , transport :: forall i. (Controller i) => StateT Context (Reader (Domain p i)) t
    , implementation :: StateT Context (Reader (Domain p i1)) t -> StateT Context (Reader (Domain p i2)) i2
    }
