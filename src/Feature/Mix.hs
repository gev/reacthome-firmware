{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
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
import           Endpoint.ATS
import qualified Endpoint.DInputs            as DI
import           Endpoint.DInputsRelaysRules
import qualified Endpoint.Relays             as R
import           Feature.DInputs             (DInputs (DInputs, getDInputs, getInputs),
                                              manageDInputs, mkDInputs,
                                              syncDInputs)
import           Feature.Relays              (Relays (Relays, getGroups, getRelays),
                                              manageRelays, mkRelays, onDo,
                                              onGroup, onInit, syncRelays)
import           GHC.TypeNats
import           Interface.Flash             as F
import           Interface.GPIO.Input
import           Interface.GPIO.Output
import           Interface.MCU               as I
import           Ivory.Language
import           Ivory.Stdlib
import           Util.CRC16



data Mix = forall f. Flash f => Mix
    { relaysN    :: Int
    , relays     :: Relays
    , dinputs    :: DInputs
    , dinputsN   :: Int
    , rules      :: Rules
    , ats        :: ATS
    , etc        :: f
    , shouldInit :: Value IBool
    , transmit   :: forall n. KnownNat n
                 => Buffer n Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }



mix :: ( MonadWriter Context m
       , MonadReader (Domain p t) m
       , Transport t, Output o, Input i, Flash f
       ) => [p -> m i] -> [p -> m o] -> (p -> f) -> m Feature
mix inputs outputs etc = do
    relays       <- mkRelays outputs
    let relaysN   = length outputs
    dinputs      <- mkDInputs inputs
    let dinputsN  = length inputs
    rules        <- mkRules dinputsN relaysN
    ats          <- mkATS
    transport    <- asks D.transport
    mcu          <- asks D.mcu
    shouldInit   <- asks D.shouldInit
    let mix       = Mix { relays
                        , relaysN
                        , dinputs
                        , dinputsN
                        , rules
                        , ats
                        , etc = etc (peripherals mcu)
                        , shouldInit
                        , transmit = T.transmitBuffer transport
                        }

    addInit "mix_init" $ load mix

    addTask $ delay 10 "mix_manage" $ manage mix
    addTask $ yeld     "mix_sync"   $ sync   mix

    pure    $ Feature mix



manage :: Mix -> Ivory ('Effects (Returns ()) r (Scope s)) ()
manage Mix{..} = do
    manageDInputs  dinputs
    manageRules    rules (getDInputs dinputs) (getRelays relays) (getGroups relays) dinputsN
    -- manageATS      ats   (getDInputs dinputs) (getRelays relays)
    manageRelays   relays



sync :: Mix -> Ivory (ProcEffects s ()) ()
sync Mix{..} = do
    syncDInputs dinputs
    syncRelays  relays



instance Controller Mix where
    handle  mix@Mix{..} buff size = do
        shouldInit' <- deref shouldInit
        action <- deref $ buff ! 0
        pure [ iNot shouldInit' ==> cond_
             [ action ==? 0x00  ==> onDo    relays buff size
             , action ==? 0x02  ==> onGroup relays buff size
             , action ==? 0x03  ==> onRule  mix    buff size
             , action ==? 0x04  ==> onMode  mix    buff size
             ]
             , action ==? 0xf2  ==> onInit  relays buff size
             ]



onRule :: KnownNat n => Mix -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s ()) ()
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


onMode :: KnownNat n => Mix -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s ()) ()
onMode mix@Mix{..} buff size = do
    when (size ==? 2 ) $ do
        store (mode ats) =<< unpack buff 1
        transmit =<< message ats
        save mix



save :: Mix -> Ivory (ProcEffects s ()) ()
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
