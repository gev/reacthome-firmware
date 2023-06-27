{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Endpoint.DInputsRelaysRules where
import           Control.Monad.Writer
import           Core.Context
import           Data.Matrix
import           Data.Value
import           Ivory.Language



data Rules = Rules
    { runRulesOn  :: RunMatrix Uint8
    , runRulesOff :: RunMatrix Uint8
    , runPayload  :: RunValues Uint8
    }



mkRules :: MonadWriter Context m => Int -> Int -> m Rules
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
