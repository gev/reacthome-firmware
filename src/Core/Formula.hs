module Core.Formula where

import Control.Monad.Reader
import Control.Monad.State
import Core.Context
import Core.Domain
import Interface.MCU
import Ivory.Language

data Formula p = forall i. Formula
    { name :: String
    , model :: Uint8
    , version :: (Int, Int)
    , shouldInit :: IBool
    , mcu :: MCU p
    , quartzFrequency :: Int
    , systemFrequency :: Int
    , implementation :: StateT Context (Reader (Domain p i)) i
    }
