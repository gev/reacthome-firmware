{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.Mix where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Core.Task
import           Core.Transport
import           Feature.DInputs       (DInputs, manageDInputs, mkDInputs,
                                        syncDInputs)
import           Feature.Relays        (Relays, manageRelays, mkRelays,
                                        syncRelays)
import           Interface.GPIO.Input
import           Interface.GPIO.Output
import           Ivory.Language



data Mix = Mix
    { relays  :: Relays
    , dinputs :: DInputs
    }



mix :: ( MonadWriter Context m
       , MonadReader (Domain p t) m
       , Transport t, Output o, Input i
       ) => [p -> m i] -> [p -> m o] -> m Feature
mix inputs outputs = do
    relays  <- mkRelays  outputs
    dinputs <- mkDInputs inputs
    let mix  = Mix {relays, dinputs}
    addTask  $ delay 10 "mix_manage" $ manage mix
    addTask  $ yeld     "mix_sync"   $ sync   mix
    pure $ Feature mix



manage :: Mix -> Ivory eff ()
manage Mix{..} = do
    manageDInputs dinputs
    manageRelays  relays



sync :: Mix -> Ivory (ProcEffects s ()) ()
sync Mix{..} = do
    syncDInputs dinputs
    syncRelays  relays



instance Controller Mix where
    handle  Mix {..} = handle relays
