module Feature.RS485.RSM.Tx where

import Feature.RS485.RSM.Data
import Ivory.Language

txHandle :: RSM -> Ivory eff ()
txHandle RSM{..} = do
    store rxLock false
    store txLock false
