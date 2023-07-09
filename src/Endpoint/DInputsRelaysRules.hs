{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Endpoint.DInputsRelaysRules where

import           Control.Monad.State (MonadState)
import           Core.Context
import           Data.Matrix
import           Data.Value
import           Endpoint.DInputs    as DI
import           Endpoint.Groups
import           Endpoint.Relays
import           Ivory.Language
import           Ivory.Stdlib



data Rules = Rules
    { runRulesOn  :: RunMatrix Uint8
    , runRulesOff :: RunMatrix Uint8
    , runPayload  :: RunValues Uint8
    }



mkRules :: MonadState Context m => Int -> Int -> m Rules
mkRules n m = do
    let runRulesOn  = runMatrix  "dinputs_relays_rules_matrix_on"  0xff n m
    let runRulesOff = runMatrix  "dinputs_relays_rules_matrix_off" 0xff n m
    let runPayload  = runValues_ "dinputs_relays_message" $ 2 + 2 * m
    runRulesOn  addArea
    runRulesOff addArea
    runPayload  addArea
    pure Rules { runRulesOn, runRulesOff, runPayload }



fillPayload :: Rules -> Uint8 -> Ivory (ProcEffects s ()) ()
fillPayload Rules{..} i = runPayload $ \payload -> do
    let payload' = addrOf payload
    store (payload' ! 0) 0x03
    store (payload' ! 1) $ i + 1
    kx <- local $ ival 2
    let run :: RunMatrix Uint8 -> Ivory eff ()
        run runRules = runRules $ \rs -> arrayMap $ \jx -> do
            kx' <- deref kx
            store (payload' ! kx') =<< deref (addrOf rs ! toIx i ! jx)
            store kx $ kx' + 1
    run runRulesOff
    run runRulesOn



manageRules :: Rules -> DInputs -> Relays -> Groups -> Int -> Ivory ('Effects (Returns ()) r (Scope s)) ()
manageRules Rules{..} DInputs{..} relays groups n =
    runDInputs  $ \dis -> arrayMap $ \ix' -> do
        let ix = fromIntegral n - ix' - 1
        let di = addrOf dis ! ix
        let run :: RunMatrix Uint8 -> Ivory ('Effects (Returns ()) r (Scope s)) ()
            run runRules = runRules $ \rules -> arrayMap $ \jx -> do
                r <- deref (addrOf rules ! toIx (fromIx ix) ! jx)
                cond_ [ r ==? 0 ==> turnOffRelay relays (toIx $ 1 + fromIx jx)
                      , r ==? 1 ==> turnOnRelay' relays groups (toIx $ 1 + fromIx jx) 0
                      , r ==? 2 ==> do changed <- iNot <$> deref (di ~> DI.synced)
                                       when changed $ toggleRelay relays groups (toIx $ 1 + fromIx jx)
                      ]
        state' <- deref $ di ~> DI.state
        ifte_ state'
            (run runRulesOn )
            (run runRulesOff)
