module Core.Formula.DFU where

import Control.Monad.Reader
import Control.Monad.State
import Core.Context
import Core.Domain
import Core.Meta
import Core.Transport (LazyTransport)

data DFU p = forall i t. (LazyTransport t) => DFU
    { meta :: Meta p
    , transport :: forall i'. StateT Context (Reader (Domain p i')) t
    , implementation :: StateT Context (Reader (Domain p i)) t -> StateT Context (Reader (Domain p i)) i
    }
