{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Feature.RS485.RBUS where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Feature.RS485.RBUS.Data
import           Interface.RS485
import           Ivory.Language
import           Protocol.RS485.RBUS.Master



rbus :: (MonadWriter Context m, MonadReader (Domain p t) m)
     => [m RS485] -> m Feature
rbus rs485 = do
     let n     = length rs485
     rs       <- sequenceA rs485

     let rbus  = RBUS {rs}
     pure $ Feature rbus



instance Controller RBUS
