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

data Doppler nd ni = Doppler
    { dopplers :: Dopplers nd
    , dinputs  :: DInputs  ni
    }

doppler :: Monad m
        => m t
        -> (t -> m (Dopplers nd))
        -> (Bool -> t -> m (DInputs ni))
        -> m (Doppler nd ni)
doppler transport' dopplers' dinputs' = do
    transport <- transport'
    dopplers  <- dopplers' transport
    dinputs   <- dinputs' True transport
    pure Doppler { dopplers, dinputs }


instance KnownNat ni => Controller (Doppler nd ni) where

    handle Doppler{..} buff _ = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionGetState ==> forceSyncDInputs dinputs
              ]
