{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}

module Endpoint.DInputsRelaysRules where

import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State  (MonadState)
import           Core.Context
import qualified Core.Domain          as D
import qualified Core.Transport       as T
import           Data.Buffer
import           Data.Matrix
import           Data.Value
import           Endpoint.DInputs     as DI (DInputs (DInputs), runDInputs,
                                             state, synced)
import           Endpoint.Groups      (Groups)
import           Endpoint.Relays      (Relays, toggleRelay, turnOffRelay,
                                       turnOnRelay')
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib



type PayloadSize n = n + n + 2


data Rules ni no = Rules
    { rulesOn  :: Matrix           ni no  Uint8
    , rulesOff :: Matrix           ni no  Uint8
    , payload  :: Values (PayloadSize no) Uint8
    , synced   :: Value                   IBool
    , transmit :: forall l. KnownNat l
               => Buffer l Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }



mkRules :: ( KnownNat ni
           , KnownNat no
           , KnownNat (PayloadSize no)
           , MonadState Context m
           , MonadReader (D.Domain p i) m
           , T.Transport t
           )
        => t -> m (Rules ni no)
mkRules transport = do
    mcu       <- asks D.mcu
    rulesOn   <- matrix'  "dinputs_relays_rules_matrix_on"  0xff
    rulesOff  <- matrix'  "dinputs_relays_rules_matrix_off" 0xff
    payload   <- values_  "dinputs_relays_rules_message"
    synced    <- value    "dinputs_relays_rules_synced"  false
    pure Rules { rulesOn
               , rulesOff
               , payload
               , synced
               , transmit = T.transmitBuffer transport
               }



fillPayload :: (KnownNat ni, KnownNat no, KnownNat (PayloadSize no))
            => Rules ni no -> Uint8 -> Ivory (ProcEffects s t) ()
fillPayload Rules{..} i = do
    store (payload ! 0) 0x03
    store (payload ! 1) $ i + 1
    kx <- local $ ival 2
    let run rules = arrayMap $ \jx -> do
            kx' <- deref kx
            store (payload ! kx') =<< deref (rules ! toIx i ! jx)
            store kx $ kx' + 1
    run rulesOff
    run rulesOn



forceSyncRules :: Rules ni no -> Ivory eff ()
forceSyncRules Rules{..} = store synced false


manageRules :: (KnownNat ni, KnownNat no)
            => Rules ni no -> DInputs -> Relays -> Groups -> Ivory ('Effects (Returns ()) r (Scope s)) ()
manageRules Rules{..} DInputs{..} relays groups =
    runDInputs  $ \dis -> arrayMap $ \ix' -> do
        let n = arrayLen $ addrOf dis
        let ix = fromIntegral n - ix' - 1 -- Need for priority di rule
        let di = addrOf dis ! ix
        let run rules = arrayMap $ \jx -> do
                r <- deref (rules ! toIx (fromIx ix) ! jx)
                cond_ [ r ==? 0 ==> turnOffRelay relays (toIx $ 1 + fromIx jx)
                      , r ==? 1 ==> turnOnRelay' relays groups (toIx $ 1 + fromIx jx) 0
                      , r ==? 2 ==> do changed <- iNot <$> deref (di ~> DI.synced)
                                       when changed $ toggleRelay relays groups (toIx $ 1 + fromIx jx)
                      ]
        state' <- deref $ di ~> DI.state
        ifte_ state'
            (run rulesOn )
            (run rulesOff)



syncRules :: (KnownNat ni, KnownNat no, KnownNat (PayloadSize no)) => Rules ni no -> Ivory (ProcEffects s ()) ()
syncRules r@Rules{..} = do
    synced' <- deref synced
    let n = arrayLen  rulesOn
    when (iNot synced') $ mapM_ sync [0 .. n-1]
    store synced true
    where
        sync i = do
            fillPayload r $ fromIntegral  i
            transmit payload
