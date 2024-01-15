{-# LANGUAGE FlexibleContexts #-}

module Implementation.Dummy where
import           Control.Monad.Reader
import           Control.Monad.State
import           Core.Context
import           Core.Controller
import           Core.Domain


data Dummy = Dummy


dummy :: (MonadState Context m, MonadReader (Domain p t c) m)
     => m feature -> m Dummy
dummy feature = void feature >> pure Dummy


instance Controller Dummy
