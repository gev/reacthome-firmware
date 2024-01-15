{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Implementation.Smart.Bottom where

import           Core.Actions
import           Core.Controller
import           Feature.DInputs
import           Feature.DS18B20
import           Feature.Scd40
import           Feature.Sht21
import           Feature.Smart.Top
import           Ivory.Language
import           Ivory.Stdlib



newtype Bottom = Bottom { top :: Top}



bottom1 :: Monad m => m Top  -> (Bool -> m DInputs) -> m DS18B20 -> m Bottom
bottom1 top dinputs ds18b20 =
    ds18b20 >> dinputs True >> Bottom <$> top



bottom2 :: Monad m => m Top -> (Bool -> m DInputs) -> m DS18B20 -> m SCD40 -> m Bottom
bottom2 top dinputs ds18b20 scd40 =
    scd40 >> bottom1 top dinputs ds18b20



instance Controller Bottom where
    handle Bottom{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionSmartTop ==> onMessage top buff size
              ]
