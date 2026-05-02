module Core.Formula.DFU where

import Control.Monad.Reader
import Control.Monad.State
import Core.Context
import Core.Controller
import Core.Domain
import Core.Meta
import Core.Transport (LazyTransport)
import Interface.Flash (Flash)

data DFU p = forall f i t. (Flash f, Controller i, LazyTransport t) => DFU
    { meta :: Meta p
    , base :: f
    , transport :: forall i'. (Controller i') => StateT Context (Reader (Domain p i')) t
    , implementation :: StateT Context (Reader (Domain p i)) t -> StateT Context (Reader (Domain p i)) i
    }
