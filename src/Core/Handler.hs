module Core.Handler where

import Control.Monad.State
import Core.Context

class Handler h t where
    addHandler :: (MonadState Context m) => h t -> m ()
