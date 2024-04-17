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

doppler :: Monad m => m t -> (t -> m Dopplers) -> (Bool -> t -> m DInputs) -> m Doppler
doppler transport' dopplers' dinputs' = do
    transport <- transport'
    dopplers  <- dopplers' transport
    dinputs   <- dinputs' True transport
    pure Doppler { dopplers, dinputs }


instance Controller Doppler where

    handle Doppler{..} buff _ = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionGetState   ==> forceSyncDInputs dinputs
              ]
