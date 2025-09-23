module Implementation.Dummy where

import Core.Context
import Core.Controller
import Core.Domain

data Dummy = Dummy

dummy :: (Monad m) => m t -> (t -> m f) -> m Dummy
dummy transport feature = do
    feature =<< transport
    pure Dummy

instance Controller Dummy
