{-# LANGUAGE FlexibleContexts #-}

module Transport.PBUS where

import           Control.Monad.RWS (MonadWriter)
import           Core.Context      (Context)
import           Core.Transport

data PBUS = PBUS

instance Transport PBUS where
    transmit = undefined

pbus :: MonadWriter Context m => m PBUS
pbus = pure PBUS
