{-# LANGUAGE FlexibleContexts #-}
module Transport.UBUS where

import           Control.Monad.RWS (MonadWriter)
import           Core.Context      (Context)
import           Core.Transport

data UBUS = UBUS

instance Transport UBUS where
    transmit = undefined

ubus :: MonadWriter Context m => m UBUS
ubus = pure UBUS
