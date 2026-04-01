module Core.Formula where

import Control.Monad.Reader
import Control.Monad.State
import Core.Context
import Core.Domain
import Core.Meta

data Formula p = forall i. Formula
    { meta :: Meta p
    , implementation :: StateT Context (Reader (Domain p i)) i
    }
