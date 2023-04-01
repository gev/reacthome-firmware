{-# LANGUAGE FlexibleContexts #-}
module Feature.RS485.RBUS where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Feature         (Feature)
import           Interface.RS485



data RBUS = RBUS


rbus :: (MonadWriter Context m, MonadReader (Domain p t) m)
     => [m RS485] -> m Feature
rbus = undefined



instance Controller RBUS
