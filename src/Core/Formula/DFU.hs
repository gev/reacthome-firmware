module Core.Formula.DFU where

import Control.Monad.Reader
import Control.Monad.State
import Core.Context
import Core.Domain
import Interface.MCU
import Ivory.Language

data DFU p = forall i t. DFU
    { name :: String
    , model :: Uint8
    , version :: (Int, Int)
    , shouldInit :: IBool
    , mcu :: MCU p
    , quartzFrequency :: Int
    , systemFrequency :: Int
    , transport :: t
    , implementation :: t -> StateT Context (Reader (Domain p i)) i
    }
