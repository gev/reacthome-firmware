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



di :: Monad m => (Bool -> m DInputs) -> m DS18B20 -> m DI
di dinputs' ds18b20 = do
    ds18b20
    dinputs <-dinputs' True
    pure DI { dinputs }



instance Controller DI where
    handle DI{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionGetState ==> forceSyncDInputs dinputs
              ]
