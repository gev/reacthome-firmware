module Core.Domain where

import Control.Monad.State
import Core.Context
import Core.Meta
import Data.Value
import Ivory.Language
import Support.Cast
import Support.ReadAddr
import Support.RunAppByAddr
import Support.Serialize
import Util.String

data Domain p = Domain
    { meta :: Meta p
    , shouldInit :: Value IBool
    }

domain ::
    (MonadState Context m) =>
    Meta p ->
    m (Domain p)
domain meta = do
    addModule inclCast
    addModule inclString
    addModule inclSerialize
    addModule inclReadAddr
    addModule inclRunAppByAddr
    shouldInit <- value "should_init" meta.shouldInit
    pure Domain{meta, shouldInit}
