module Implementation.Dummy where

data Dummy = Dummy

dummy :: (Monad m) => m t -> (t -> m f) -> m Dummy
dummy transport feature = do
    feature =<< transport
    pure Dummy
