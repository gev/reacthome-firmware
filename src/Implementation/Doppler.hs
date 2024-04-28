{-# LANGUAGE GADTs           #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Implementation.Doppler where

import           Core.Actions
import           Core.Controller
import           Feature.DInputs
import           Feature.Dopplers
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib

data Doppler n = Doppler
    { dopplers :: Dopplers
    , dinputs  :: DInputs n
    }

doppler :: (KnownNat n, Monad m) => m t -> (t -> m Dopplers) -> (Bool -> t -> m (DInputs n)) -> m (Doppler n)
doppler transport' dopplers' dinputs' = do
    transport <- transport'
    dopplers  <- dopplers' transport
    dinputs   <- dinputs' True transport
    pure Doppler { dopplers, dinputs }


instance KnownNat n => Controller (Doppler n) where

    handle Doppler{..} buff _ = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionGetState ==> forceSyncDInputs dinputs
              ]
