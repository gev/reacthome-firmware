{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Implementation.Mix where

import           Control.Monad               (zipWithM_)
import           Control.Monad.Reader        (MonadReader, asks)
import           Control.Monad.State         (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Controller
import           Core.Domain                 as D
import           Core.Task
import           Core.Transport
import qualified Core.Transport              as T
import           Data.Buffer
import           Data.Matrix
import           Data.Serialize
import           Data.Value
import           Endpoint.ATS                as A
import qualified Endpoint.DInputs            as DI
import           Endpoint.DInputsRelaysRules
import qualified Endpoint.Groups             as G
import qualified Endpoint.Relays             as R
import           Feature.DInputs             as FDI (DInputs (DInputs, getDInputs, getInputs),
                                                     dinputs, forceSyncDInputs,
                                                     manageDInputs, n,
                                                     syncDInputs)
import           Feature.Mix.Indicator       as FI (Indicator, indicator,
                                                    onFindMe)
import           Feature.Relays              as FR (Relays (Relays, getGroups, getRelays),
                                                    forceSyncRelays,
                                                    manageRelays, n, onDo,
                                                    onGroup, onInit, relays,
                                                    syncRelays)
import           GHC.RTS.Flags               (DebugFlags (stable))
import           GHC.TypeNats
import           Interface.Display
import           Interface.Flash             as F
import           Interface.GPIO.Input
import           Interface.GPIO.Output
import           Interface.GPIO.Port
import           Interface.MCU               as I
import           Ivory.Language
import           Ivory.Stdlib
import           Prelude                     hiding (error)
import           Util.CRC16



data Mix = forall f. Flash f => Mix
    { relaysN    :: Int
    , relays     :: Relays
    , dinputs    :: DInputs
    , dinputsN   :: Int
    , rules      :: Rules
    , ats        :: ATS
    , indicator  :: Indicator
    , etc        :: f
    , shouldInit :: Value IBool
    , transmit   :: forall n. KnownNat n
                 => Buffer n Uint8 -> forall s t. Ivory (ProcEffects s t) ()
    }



mix :: ( MonadState Context m
       , MonadReader (Domain p c) m
       , Transport t
       , Flash f
       ) => m t -> (Bool -> t -> m DInputs) -> (t -> m Relays) -> (ATS -> DI.DInputs -> R.Relays -> t -> m Indicator) -> (p -> f) -> m Mix
mix transport' dinputs' relays' indicator' etc = do
    transport    <- transport'
    relays       <- relays' transport
    let relaysN   = FR.n relays
    dinputs      <- dinputs' True transport
    let dinputsN  = FDI.n dinputs
    rules        <- mkRules transport dinputsN relaysN
    ats          <- mkATS transport
    indicator    <- indicator' ats (getDInputs dinputs) (getRelays relays) transport
    mcu          <- asks D.mcu
    shouldInit   <- asks D.shouldInit
    let mix       = Mix { relays
                        , relaysN
                        , dinputs
                        , dinputsN
                        , rules
                        , ats
                        , indicator
                        , etc = etc (peripherals mcu)
                        , shouldInit
                        , transmit = T.transmitBuffer transport
                        }

    addInit "mix" $ load mix

    addTask $ delay 10 "mix_manage" $ manage mix
    addTask $ delay  1 "mix_sync"   $ sync   mix

    addSync "dinputs" $ forceSyncDInputs dinputs
    addSync "relays"  $ forceSyncRelays  relays
    addSync "rules"   $ forceSyncRules   rules
    addSync "ats"     $ forceSyncATS     ats

    pure mix



manage :: Mix -> Ivory ('Effects (Returns ()) r (Scope s)) ()
manage Mix{..} = do
    manageDInputs  dinputs
    manageRules    rules (getDInputs dinputs) (getRelays relays) (getGroups relays)
    manageATS      ats   (getDInputs dinputs) (getRelays relays)
    manageRelays   relays



sync :: Mix -> Ivory (ProcEffects s ()) ()
sync Mix{..} = do
    syncDInputs dinputs
    syncRelays  relays
    syncRules   rules
    syncATS     ats



instance Controller Mix where
    handle  mix@Mix{..} buff size = do
        shouldInit' <- deref shouldInit
        action <- deref $ buff ! 0
        cond_ [ action ==? actionDo          .&& iNot shouldInit' ==> onDo       relays    buff size
              , action ==? actionGroup       .&& iNot shouldInit' ==> onGroup    relays    buff size
              , action ==? actionDiRelaySync .&& iNot shouldInit' ==> onRule     mix       buff size
              , action ==? actionMix         .&& iNot shouldInit' ==> onMode     mix       buff size
              , action ==? actionInitialize                       ==> onInit     relays    buff size
              , action ==? actionGetState                         ==> onGetState mix
              , action ==? actionFindMe                           ==> onFindMe   indicator buff size
              , action ==? actionError                            ==> resetError ats
              ]



onRule :: KnownNat n => Mix -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onRule mix@Mix{..} buff size = do
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
        save mix


onMode :: KnownNat n => Mix -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onMode mix@Mix{..} buff size = do
    when (size ==? 2 ) $ do
        store (mode ats) =<< unpack buff 1
        manageLock mix
        resetError ats
        save mix



onGetState :: Mix -> Ivory eff ()
onGetState Mix{..} = do
    forceSyncDInputs dinputs
    forceSyncRules rules
    forceSyncATS ats
    initialized <- iNot <$> deref shouldInit
    when initialized $ do
        forceSyncRelays relays



save :: Mix -> Ivory (ProcEffects s t) ()
save Mix{..} = do
    erasePage etc
    crc   <- local $ istruct initCRC16
    mode' <- deref (mode ats)
    updateCRC16 crc mode'
    F.write etc 0 $ safeCast mode'
    kx <- local $ ival 4
    let run :: (Rules -> RunMatrix Uint8) -> Ivory eff ()
        run runRules = runRules rules $ \rs -> arrayMap $ \ix -> arrayMap $ \jx -> do
            kx' <- deref kx
            v   <- deref (addrOf rs ! ix ! jx)
            updateCRC16 crc v
            F.write etc kx' $ safeCast v
            store kx $ kx' + 4
    run runRulesOff
    run runRulesOn
    kx' <- deref kx
    F.write etc kx' . safeCast =<< deref (crc ~> msb)
    F.write etc (kx' + 4) . safeCast =<< deref (crc ~> lsb)



load :: Mix -> Ivory (ProcEffects s ()) ()
load mix@Mix{..} = do
    valid <- checkCRC mix
    when valid $ do
        store (mode ats) . castDefault =<< F.read etc 0
        manageLock mix
        kx <- local $ ival 4
        let run :: (Rules -> RunMatrix Uint8) -> Ivory eff ()
            run runRules = runRules rules $ \rs -> arrayMap $ \ix -> arrayMap $ \jx -> do
                kx' <- deref kx
                store (addrOf rs ! ix ! jx) . castDefault =<< F.read etc kx'
                store kx $ kx' + 4
        run runRulesOff
        run runRulesOn


checkCRC :: Mix -> Ivory (ProcEffects s ()) IBool
checkCRC Mix{..} = do
    crc   <- local $ istruct initCRC16
    updateCRC16 crc . castDefault =<< F.read etc 0
    kx <- local $ ival 4
    times (fromIntegral $ 2 * dinputsN * relaysN :: Ix 256) $ \_ -> do
        kx' <- deref kx
        updateCRC16 crc . castDefault =<< F.read etc kx'
        store kx $ kx' + 4
    kx'   <- deref kx
    msb'  <- castDefault <$> F.read etc kx'
    lsb'  <- castDefault <$> F.read etc (kx' + 4)
    lsb'' <- deref $ crc ~> lsb
    msb'' <- deref $ crc ~> msb
    pure $ lsb' ==? lsb'' .&& msb' ==? msb''



manageLock Mix{..} = R.runRelays (getRelays relays) $ \r -> do
    let r' = addrOf r
    mode' <- deref $ mode ats
    arrayMap $ \ix -> store (r' ! ix ~> R.lock) false
    cond_ [ mode' ==? mode_N1_G ==> do
                store (r' ! 0 ~> R.lock) true
                store (r' ! 4 ~> R.lock) true
                store (r' ! 5 ~> R.lock) true
          , mode' ==? mode_N2   ==> do
                store (r' ! 0 ~> R.lock) true
                store (r' ! 1 ~> R.lock) true
          , mode' ==? mode_N2_G ==> do
                store (r' ! 0 ~> R.lock) true
                store (r' ! 1 ~> R.lock) true
                store (r' ! 4 ~> R.lock) true
                store (r' ! 5 ~> R.lock) true
          ]
