{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Endpoint.DInputsRelaysRules where

import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State  (MonadState)
import           Core.Context
import qualified Core.Domain          as D
import qualified Core.Transport       as T
import           Data.Buffer
import           Data.Matrix
import           Data.Value
import           Endpoint.DInputs     as DI
import           Endpoint.Groups
import           Endpoint.Relays
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib



data Rules = Rules
    { n           :: Int
    , runRulesOn  :: RunMatrix Uint8
    , runRulesOff :: RunMatrix Uint8
    , runPayload  :: RunValues Uint8
    , synced      :: Value     IBool
    , transmit    :: forall n. KnownNat n
                  => Buffer n Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }



mkRules :: (MonadState Context m, MonadReader (D.Domain p t i) m, T.Transport t)
        => Int -> Int -> m Rules
mkRules n m = do
    mcu             <- asks D.mcu
    transport       <- asks D.transport
    let runRulesOn   = runMatrix  "dinputs_relays_rules_matrix_on"  0xff n m
    let runRulesOff  = runMatrix  "dinputs_relays_rules_matrix_off" 0xff n m
    let runPayload   = runValues_ "dinputs_relays_rules_message"  $ 2 + 2 * m
    synced          <- value      "dinputs_relays_rules_synced"     false
    runRulesOn  addArea
    runRulesOff addArea
    runPayload  addArea
    pure Rules { n
               , runRulesOn
               , runRulesOff
               , runPayload
               , synced
               , transmit = T.transmitBuffer transport
               }



fillPayload :: Rules -> Uint8 -> Ivory (ProcEffects s t) ()
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



forceSyncRules :: Rules -> Ivory eff ()
forceSyncRules Rules{..} = store synced false


manageRules :: Rules -> DInputs -> Relays -> Groups -> Ivory ('Effects (Returns ()) r (Scope s)) ()
manageRules Rules{..} DInputs{..} relays groups =
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



syncRules :: Rules -> Ivory (ProcEffects s ()) ()
syncRules r@Rules{..} = do
    synced' <- deref synced
    when (iNot synced') $ mapM_ sync [0 .. n-1]
    store synced true
    where
        sync i = runPayload $ \p -> do
            fillPayload r $ fromIntegral  i
            transmit $ addrOf p
