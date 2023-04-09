{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Feature.RS485.RBUS where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Core.Handler
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Feature.RS485.RBUS.Data
import           Feature.RS485.RBUS.Rx
import           Feature.RS485.RBUS.Tx
import           Interface.RS485
import           Ivory.Language
import           Protocol.RS485.RBUS.Master



rbus :: (MonadWriter Context m, MonadReader (Domain p t) m)
     => [m RS485] -> m Feature
rbus rs485 = do
     let n     = length rs485
     rbus  <- zipWithM rbus' rs485 (iterate (+1) 1)
     pure $ Feature rbus



rbus' :: (MonadWriter Context m, MonadReader (Domain p t) m)
     => m RS485 -> Uint8 -> m RBUS
rbus' rs485 n = do
     rs       <- rs485
     let rbus  = RBUS {n, rs}
     addHandler $ HandleRS485 rs (rxHandle rbus) (txHandle rbus)
     pure rbus


instance Controller [RBUS]
