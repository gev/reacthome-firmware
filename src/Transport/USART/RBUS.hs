{-# LANGUAGE FlexibleContexts #-}

module Transport.USART.RBUS where

import           Control.Monad.RWS (MonadWriter)
import           Core.Context      (Context)
import           Core.Transport

data RBUS = RBUS

instance Transport RBUS where
    transmit = undefined

rbus :: MonadWriter Context m => m RBUS
rbus = pure RBUS
