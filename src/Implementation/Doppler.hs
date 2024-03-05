{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Implementation.Doppler where

import           Core.Actions
import           Core.Controller  
import           Feature.DInputs
import           Feature.Dopplers
import           Ivory.Language
import           Ivory.Stdlib

data Doppler = Doppler
    { dopplers :: Dopplers
    , dinputs  :: DInputs
    }

doppler :: Monad m => m Dopplers -> (Bool -> m DInputs) -> m Doppler
doppler dopplers' dinputs' = do
    dopplers <- dopplers'
    dinputs  <- dinputs' True
    pure Doppler { dopplers, dinputs }


instance Controller Doppler where

    handle Doppler{..} buff _ = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionGetState   ==> forceSyncDInputs dinputs
              ]
