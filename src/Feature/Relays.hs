{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.Relays where

import           Control.Monad.Reader    (Reader, asks)
import           Core.Controller
import qualified Core.Domain             as D
import           Core.Feature
import           Core.Include
import           Core.Initialize
import           Core.Task
import qualified Core.Transport          as T
import           Data.Buffer
import           Data.Index
import           Data.List               (group)
import           Data.Record
import           Data.Serialize
import           Data.Value
import qualified Endpoint.Groups         as G
import qualified Endpoint.Relays         as R
import           GHC.TypeNats
import           Interface.GPIO.Output
import           Interface.GPIOs.Outputs as I
import           Interface.MCU           (MCU, systemClock)
import           Interface.SystemClock   (SystemClock, getSystemTime)
import           Ivory.Language
import           Ivory.Stdlib
import           Transport.RBUS.Data     (RBUS (rs))



data Relays = forall os. Outputs os => Relays
    { n          :: Uint8
    , getRelays  :: R.Relays
    , getGroups  :: G.Groups
    , getOutputs :: os
    , shouldInit :: Value IBool
    , clock      :: SystemClock
    , current    :: Index Uint8
    , transmit   :: forall n. KnownNat n
                 => Buffer n Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }



relays :: (MakeOutputs o os, T.Transport t, MCU mcu)
       => [mcu -> o] -> Reader (D.Domain mcu t) Feature
relays outs = do
    mcu        <- asks D.mcu
    transport  <- asks D.transport
    shouldInit <- asks D.shouldInit
    let os = ($ mcu) <$> outs
    let n = length os
    pure . Feature $ Relays { n = fromIntegral n
                            , getRelays  = R.relays "relays" n
                            , getGroups  = G.groups "groups" n
                            , getOutputs = makeOutputs "relays_outputs" os
                            , shouldInit = shouldInit
                            , clock      = systemClock mcu
                            , current    = index "current_relay"
                            , transmit   = T.transmit transport
                            }



instance Include Relays where
    include (Relays {getRelays, getGroups, getOutputs, current}) = do
        include getOutputs
        include getRelays
        include getGroups
        include current


instance Initialize Relays where
    initialize (Relays {getOutputs}) =
        initialize getOutputs



instance Task Relays where
    tasks rs = [ delay 10 "relays_manage" $ manage rs
               , delay  5 "relays_sync"   $ sync rs
               ]



manage :: Relays -> Ivory eff ()
manage (Relays {getRelays, getGroups, getOutputs, clock}) = do
    R.runRelays getRelays $ \rs -> do
        arrayMap $ \ix -> do
            isOn <- R.getState rs ix
            ifte_ isOn
                    (do
                        I.set getOutputs $ toIx $ fromIx ix
                        delay <- R.getDelayOff rs ix
                        when (delay >? 0) $ do
                            t1 <- getSystemTime clock
                            t0 <- R.getTimestamp rs ix
                            when (t1 >=? t0 + delay ) $ do
                                R.setState    rs ix false
                                R.setDelayOff rs ix 0
                                R.setSynced   rs ix false
                    )
                    (do
                        I.reset getOutputs $ toIx $ fromIx ix
                        delay <- R.getDelayOn rs ix
                        when (delay >? 0) $ do
                            t1 <- getSystemTime clock
                            t0 <- R.getTimestamp rs ix
                            when (t1 >=? t0 + delay ) $ do
                                R.setState     rs ix true
                                R.setDelayOn   rs ix 0
                                R.setTimestamp rs ix t1
                                R.setSynced    rs ix false
                    )



sync :: Relays -> Ivory (ProcEffects s ()) ()
sync rs@(Relays {n, current}) = do
    let current' = addrOf current
    i <- deref current'
    syncRelays rs i
    syncGroups rs i
    store current' $ i + 1



syncRelays :: Relays -> Uint8 -> Ivory (ProcEffects s ()) ()
syncRelays (Relays {n, getRelays, transmit}) i =
    R.runRelays getRelays $ \rs -> do
        let ix = toIx i
        synced <- R.getSynced rs ix
        when (iNot synced) $ do
            transmit =<< R.message getRelays (i .% n)
            R.setSynced rs ix true



syncGroups :: Relays -> Uint8 -> Ivory (ProcEffects s ()) ()
syncGroups (Relays {n, getGroups, transmit}) i =
    G.runGroups getGroups $ \gs -> do
        let ix = toIx i
        synced <- G.getSynced gs ix
        when (iNot synced) $ do
            transmit =<< G.message getGroups (i .% n)
            G.setSynced gs ix true



instance Controller Relays where
    handle rs buff size = do
        let buff' = addrOf buff
        shouldInit' <- deref . addrOf $ shouldInit rs
        pure [ size >=? 3 ==> do
                action <- deref $ buff' ! 0
                cond_ [ iNot shouldInit' ==> cond_
                      [ action ==? 0x00  ==> onDo    rs buff' size
                      , action ==? 0x02  ==> onGroup rs buff' size
                      ]
                      , action ==? 0xf2  ==> onInit  rs buff' size
                      ]
             ]



onDo :: KnownNat l
     => Relays
     -> Ref Global ('Array l ('Stored Uint8))
     -> Uint8
     -> Ivory (ProcEffects s ()) ()
onDo (Relays {n, clock, getRelays, getGroups}) buff size = do
    index <- deref $ buff ! 1
    when (index >=? 1 .&& index <=? n) $
        R.runRelays getRelays $ \rs -> do
            let ix = toIx $ index - 1
            action <- deref $ buff ! 2
            cond_ [ action ==? 0 ==> do
                        R.setState  rs ix false
                        R.setSynced rs ix false
                  , action ==? 1 ==> do
                        R.setTimestamp rs ix =<< getSystemTime clock
                        shouldDelay <- syncGroup getGroups rs ix
                        cond_ [ size >=? 7 ==> do R.setDelayOff rs ix =<< unpackLE buff 3
                              , true       ==> do R.setDelayOff rs ix =<< R.getDefaultDelayOff rs ix
                              ]
                        when (iNot shouldDelay) $ do
                            R.setState     rs ix true
                            R.setSynced    rs ix false
                  , action ==? 2 .&& size >=? 7 ==> do
                        R.setDefaultDelay   rs ix =<< unpackLE buff 3
                        R.setSynced         rs ix false
                  , action ==? 3 ==> do
                        group <- unpack buff 3
                        turnOffGroup rs ix group
                        R.setGroup   rs ix group
                        R.setSynced  rs ix false
                  ]


syncGroup :: KnownNat n
          => G.Groups
          -> Records n R.RelayStruct
          -> Ix n
          -> Ivory (ProcEffects s ()) IBool
syncGroup groups rs ix = do
    group <- R.getGroup rs ix
    shouldDelay <- turnOffGroup rs ix group
    when shouldDelay $
        G.runGroups groups $ \gs -> do
            delay <- G.getDelay gs (toIx $ group - 1)
            R.setDelayOn rs ix $ delay + 1
    pure shouldDelay


turnOffGroup :: KnownNat n
             => Records n R.RelayStruct
             -> Ix n
             -> Uint8
             -> Ivory (ProcEffects s ()) IBool
turnOffGroup rs ix g = do
    f <- local $ ival false
    arrayMap $ \jx -> do
        when (jx /=? ix) $ do
            group <- R.getGroup rs jx
            when (group ==? g) $ do
                isOn  <- R.getState   rs jx
                delay <- R.getDelayOn rs jx
                when (isOn .|| delay >? 0) $ do
                    R.setState   rs jx false
                    R.setDelayOn rs jx 0
                    R.setSynced  rs jx false
                    store f true
    deref f



onGroup :: KnownNat l
        => Relays
        -> Ref Global ('Array l ('Stored Uint8))
        -> Uint8
        -> Ivory eff ()
onGroup (Relays {n, getGroups}) buff size =
    when (size >=? 7) $ do
        index <- unpack buff 1
        when (index >=? 1 .&& index <=? n) $
            G.runGroups getGroups $ \gs -> do
                let ix = toIx $ index - 1
                G.setEnabled gs ix =<< unpack   buff 2
                G.setDelay   gs ix =<< unpackLE buff 3
                G.setSynced  gs ix false



{-
    TODO: Generalize initialization
-}
onInit :: KnownNat l
        => Relays
        -> Ref Global ('Array l ('Stored Uint8))
        -> Uint8
        -> Ivory (ProcEffects s ()) ()
onInit rs@(Relays {n, clock, getRelays, getGroups, shouldInit}) buff size =
    when (size >=? 5 * n + 6 * n) $ do
        j <- local $ ival 1
        initGroups rs buff j
        initRelays rs buff j
        store (addrOf shouldInit) false



initGroups :: KnownNat n
           => Relays
           -> Ref Global (Array n ('Stored Uint8))
           -> Ref s (Stored (Ix n))
           -> Ivory eff ()
initGroups (Relays {getGroups}) buff j =
    G.runGroups getGroups $ \gs ->
        arrayMap $ \ix -> do
            j' <- deref j
            G.setEnabled      gs ix =<< unpack   buff  j'
            G.setDelay        gs ix =<< unpackLE buff (j' + 1)
            store j $ j' + 5



initRelays :: KnownNat n
           => Relays
           -> Ref Global (Array n ('Stored Uint8))
           -> Ref s (Stored (Ix n))
           -> Ivory eff ()
initRelays (Relays {clock, getRelays}) buff j =
    R.runRelays getRelays $ \rs ->
        arrayMap $ \ix -> do
            j' <- deref j
            R.setState        rs ix =<< unpack   buff  j'
            R.setGroup        rs ix =<< unpack   buff (j' + 1)
            R.setDelayOff        rs ix =<< unpackLE buff (j' + 2)
            R.setDefaultDelay rs ix =<< unpackLE buff (j' + 2)
            R.setTimestamp    rs ix =<< getSystemTime clock
            store j $ j' + 6
