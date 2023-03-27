{-# LANGUAGE FlexibleContexts #-}

module Protocol.RS485.RBUS.Master.MacTable where

import           Control.Monad.RWS
import           Core.Context



data MacTable = MacTable



macTable :: MonadWriter Context m => m MacTable
macTable = pure MacTable
