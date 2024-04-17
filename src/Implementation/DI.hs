{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Implementation.DI where

import           Control.Monad
import           Core.Actions
import           Core.Controller
import           Feature.DInputs (DInputs, forceSyncDInputs)
import           Feature.DS18B20
import           Ivory.Language
import           Ivory.Stdlib



newtype DI = DI { dinputs :: DInputs }



di :: Monad m => m t -> (Bool -> t -> m DInputs) -> (t -> m DS18B20) -> m DI
di transport' dinputs' ds18b20 = do
    transport <- transport'
    ds18b20 transport
    dinputs <-dinputs' True transport
    pure DI { dinputs }



instance Controller DI where
    handle DI{..} buff _ = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionGetState ==> forceSyncDInputs dinputs
              ]
