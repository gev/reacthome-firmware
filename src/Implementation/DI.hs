{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
module Implementation.DI where

import           Control.Monad
import           Control.Monad.Reader  (MonadReader)
import           Control.Monad.RWS     (asks)
import           Control.Monad.State   (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Task
import           Core.Transport
import           Data.Value
import           Feature.ALED
import           Feature.DInputs       (DInputs, forceSyncDInputs)
import           Feature.DS18B20
import           GHC.TypeNats
import           Interface.GPIO.Output
import           Interface.GPIO.Port
import           Interface.MCU         (peripherals)
import           Ivory.Language
import           Ivory.Stdlib


newtype DI n = DI { dinputs :: DInputs n }



di :: Monad m => m t -> (Bool -> t -> m (DInputs n)) -> (t -> m DS18B20) -> m (DI n)
di transport' dinputs' ds18b20 = do
    transport <- transport'
    ds18b20 transport
    dinputs <- dinputs' True transport
    pure DI { dinputs }



instance KnownNat n => Controller (DI n) where
    handle d@DI{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionGetState ==> forceSyncDInputs dinputs
              ]
