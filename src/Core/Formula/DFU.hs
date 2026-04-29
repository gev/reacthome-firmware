module Core.Formula.DFU where

import Control.Monad.Reader
import Control.Monad.State
import Core.Context
import Core.Controller (OnMessage)
import Core.Domain
import Core.Meta
import Core.Transport (LazyTransport)

data DFU p = forall l i t. (LazyTransport t) => DFU
    { meta :: Meta p
    , transport :: forall i'. OnMessage l -> StateT Context (Reader (Domain p i')) t
    , implementation :: (OnMessage l -> StateT Context (Reader (Domain p i)) t) -> StateT Context (Reader (Domain p i)) i
    }
