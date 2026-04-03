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

data Domain p i = Domain
    { meta :: Meta p
    , shouldInit :: Value IBool
    , implementation :: i
    }

domain ::
    (MonadState Context m) =>
    Meta p ->
    i ->
    m (Domain p i)
domain meta implementation = do
    addModule inclCast
    addModule inclString
    addModule inclSerialize
    addModule inclReadAddr
    addModule inclRunAppByAddr
    shouldInit <- value "should_init" meta.shouldInit
    pure Domain{meta, shouldInit, implementation}
