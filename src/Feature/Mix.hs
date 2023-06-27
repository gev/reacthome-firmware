{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.Mix where

import           Control.Monad               (zipWithM_)
import           Control.Monad.Reader        (MonadReader, asks)
import           Control.Monad.Writer        (MonadWriter)
import           Core.Context
import           Core.Controller
import           Core.Domain                 as D
import           Core.Feature
import           Core.Task
import           Core.Transport
import qualified Core.Transport              as T
import           Data.Buffer
import           Data.Matrix
import           Data.Serialize
import           Data.Value
import qualified Endpoint.DInputs            as DI
import           Endpoint.DInputsRelaysRules
import           Feature.DInputs             (DInputs (DInputs, getDInputs, getInputs),
                                              manageDInputs, mkDInputs,
                                              syncDInputs)
import           Feature.Relays              (Relays (Relays), manageRelays,
                                              mkRelays, offRelay, onDo, onGroup,
                                              onInit, onRelay, syncRelays,
                                              toggleRelay)
import           GHC.TypeNats
import           Interface.GPIO.Input
import           Interface.GPIO.Output
import           Ivory.Language
import           Ivory.Stdlib



data Mix = Mix
    { relaysN    :: Int
    , relays     :: Relays
    , dinputs    :: DInputs
    , dinputsN   :: Int
    , rules      :: Rules
    , shouldInit :: Value IBool
    , transmit   :: forall n. KnownNat n
                 => Buffer n Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }



mix :: ( MonadWriter Context m
       , MonadReader (Domain p t) m
       , Transport t, Output o, Input i
       ) => [p -> m i] -> [p -> m o] -> m Feature
mix inputs outputs = do
    relays       <- mkRelays outputs
    let relaysN   = length outputs
    dinputs      <- mkDInputs inputs
    let dinputsN  = length inputs
    rules        <- mkRules dinputsN relaysN
    transport    <- asks D.transport
    shouldInit   <- asks D.shouldInit
    let mix       = Mix { relays
                        , relaysN
                        , dinputs
                        , dinputsN
                        , rules
                        , shouldInit
                        , transmit = T.transmitBuffer transport
                        }
    addTask $ delay 10 "mix_manage" $ manage mix
    addTask $ yeld     "mix_sync"   $ sync   mix
    pure    $ Feature mix



manage :: Mix -> Ivory ('Effects (Returns ()) r (Scope s)) ()
manage Mix{..} = do
    manageDInputs  dinputs
    manageMixRules rules dinputs relays dinputsN
    manageRelays   relays



manageMixRules :: Rules -> DInputs -> Relays -> Int -> Ivory ('Effects (Returns ()) r (Scope s)) ()
manageMixRules Rules{..} DInputs{..} relays n = zipWithM_ zip (reverse getInputs) [n, n-1 .. 1]
    where
        zip :: Input i => i -> Int -> Ivory ('Effects (Returns ()) r (Scope s)) ()
        zip input i = DI.runDInputs getDInputs $ \dis -> do
            let ix = i - 1
            let di = addrOf dis ! fromIntegral ix
            let run :: RunMatrix Uint8 -> Ivory ('Effects (Returns ()) r (Scope s)) ()
                run runRules = runRules $ \rules -> arrayMap $ \jx -> do
                    r <- deref (addrOf rules ! fromIntegral ix ! jx)
                    cond_ [ r ==? 0 ==> offRelay relays (toIx . fromIx $ jx + 1)
                          , r ==? 1 ==> onRelay  relays (toIx . fromIx $ jx + 1) 0
                          , r ==? 2 ==> do
                                    changed <- iNot <$> deref (di ~> DI.synced)
                                    when changed $ toggleRelay relays (toIx . fromIx $ jx + 1)
                          ]
            state' <- deref $ di ~> DI.state
            ifte_ state'
                (run runRulesOn )
                (run runRulesOff)



sync :: Mix -> Ivory (ProcEffects s ()) ()
sync Mix{..} = do
    syncDInputs dinputs
    syncRelays  relays



instance Controller Mix where
    handle  mix@Mix{..} buff size = do
        shouldInit' <- deref shouldInit
        pure [ size >=? 3 ==> do
                action <- deref $ buff ! 0
                cond_ [ iNot shouldInit' ==> cond_
                      [ action ==? 0x00  ==> onDo    relays buff size
                      , action ==? 0x02  ==> onGroup relays buff size
                      , action ==? 0x03  ==> onRule  mix    buff size
                      ]
                      , action ==? 0xf2  ==> onInit  relays buff size
                      ]
             ]



onRule :: KnownNat n => Mix -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s ()) ()
onRule Mix{..} buff size = do
    let relaysN'  = fromIntegral relaysN
    let dinputsN' = fromIntegral dinputsN
    i <- subtract 1 <$> deref (buff ! 1)
    when (size ==? 2 + 2 * relaysN' .&& i <? dinputsN') $ do
        kx <- local $ ival 2
        let run :: (Rules -> RunMatrix Uint8) -> Ivory eff ()
            run runRules = runRules rules $ \rs -> arrayMap $ \jx -> do
                kx' <- deref kx
                store (addrOf rs ! toIx i ! jx) =<< unpack buff kx'
                store kx $ kx' + 1
        run runRulesOff
        run runRulesOn
        fillPayload rules i
        runPayload rules $ \p -> transmit . addrOf $ p
