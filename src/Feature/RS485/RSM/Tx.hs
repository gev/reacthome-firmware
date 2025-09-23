module Feature.RS485.RSM.Tx where

import Control.Monad (zipWithM_)
import Data.Buffer
import Data.Queue
import Feature.RS485.RSM.Data
import GHC.TypeNats
import Interface.Mac
import qualified Interface.RS485 as RS
import Interface.SystemClock
import Ivory.Language
import Ivory.Stdlib

txHandle :: RSM -> Ivory eff ()
txHandle RSM{..} = do
    store rxLock false
    store txLock false
