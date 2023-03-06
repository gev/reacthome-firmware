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
               , delay  1 "relays_sync"   $ sync rs
               ]



manage :: Relays -> Ivory eff ()
manage (Relays {getRelays, getGroups, getOutputs, clock}) = do
    R.runRelays getRelays $ \rs -> do
        let rs' = addrOf rs
        arrayMap $ \ix -> do
            isOn <- deref $ rs' ! ix ~> R.state
            ifte_ isOn
                    (do
                        I.set getOutputs $ toIx $ fromIx ix
                        delay <- R.getDelay getRelays $ toIx $ fromIx ix
                        when (delay >? 0) $ do
                            t1 <- getSystemTime clock
                            t0 <- R.getTimestamp getRelays $ toIx $ fromIx ix
                            when (t1 >=? t0 + delay ) $ do
                                R.turnOff         getRelays $ toIx $ fromIx ix
                                R.setSynced false getRelays $ toIx $ fromIx ix
                    )
                    (do
                        I.reset getOutputs $ toIx $ fromIx ix
                    )



sync :: Relays -> Ivory (ProcEffects s ()) ()
sync rs@(Relays {n, current}) = do
    let current' = addrOf current
    i <- deref current'
    syncRelays rs i
    syncGroups rs i
    store current' $ i + 1

syncRelays :: Relays -> Uint8 -> Ivory (ProcEffects s ()) ()
syncRelays (Relays {n, getRelays, transmit}) i = do
    synced <- R.getSynced getRelays $ toIx i
    when (iNot synced) $ do
        transmit =<< R.message getRelays (i .% n)
        R.setSynced true getRelays $ toIx i

syncGroups :: Relays -> Uint8 -> Ivory (ProcEffects s ()) ()
syncGroups (Relays {n, getGroups, transmit}) i = do
    synced <- G.getSynced getGroups $ toIx i
    when (iNot synced) $ do
        transmit =<< G.message getGroups (i .% n)
        G.setSynced true getGroups $ toIx i



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
onDo (Relays {n, clock, getRelays}) buff size = do
    index <- deref $ buff ! 1
    when (index >=? 1 .&& index <=? n) $ do
        let index' = index - 1
        action <- deref $ buff ! 2
        cond_ [ action ==? 0 ==> do
                    R.turnOff         getRelays $ toIx index'
                    R.setSynced false getRelays $ toIx index'
              , action ==? 1 ==> do
                    ts <- getSystemTime clock
                    cond_
                        [ size >=? 7 ==> do
                            delay <- unpackLE buff 3
                            R.turnOn delay    getRelays $ toIx index'
                            R.setTimestamp ts getRelays $ toIx index'
                            R.setSynced false getRelays $ toIx index'
                        , true ==> do
                            R.turnOn_         getRelays $ toIx index'
                            R.setTimestamp ts getRelays $ toIx index'
                            R.setSynced false getRelays $ toIx index'
                        ]
              , action ==? 2 .&& size >=? 7 ==> do
                    delay <- unpackLE buff 3
                    R.setDefaultDelay delay getRelays $ toIx index'
                    R.setSynced       false getRelays $ toIx index'
              , action ==? 3 ==> do
                    group <- unpack buff 3
                    R.setGroup  group getRelays $ toIx index'
                    R.setSynced false getRelays $ toIx index'
              ]



onGroup :: KnownNat l
        => Relays
        -> Ref Global ('Array l ('Stored Uint8))
        -> Uint8
        -> Ivory (ProcEffects s ()) ()
onGroup (Relays {n, getGroups}) buff size =
    when (size >=? 7) $ do
        index   <- unpack   buff 1
        when (index >=? 1 .&& index <=? n) $ do
            let index' = index - 1
            enabled <- unpack   buff 2
            delay   <- unpackLE buff 3
            G.setState enabled delay getGroups $ toIx index'
            G.setSynced false        getGroups $ toIx index'



{-
    TODO: Generalize initialization
-}
onInit :: KnownNat l
        => Relays
        -> Ref Global ('Array l ('Stored Uint8))
        -> Uint8
        -> Ivory (ProcEffects s ()) ()
onInit (Relays {n, clock, getRelays, getGroups, shouldInit}) buff size =
    when (size >=? 5 * n + 6 * n) $ do
        j <- local $ ival 1
        G.runGroups getGroups $ \gs -> do
            let gs' = addrOf gs
            arrayMap $ \ix -> do
                j' <- deref j
                store (gs' ! ix ~> G.enabled) =<< unpack   buff  j'
                store (gs' ! ix ~> G.delay)   =<< unpackLE buff (j' + 1)
                store j $ j' + 5
        j' <- deref j
        R.runRelays getRelays $ \rs -> do
            let state = addrOf rs
            arrayMap $ \ix -> do
                j' <- deref j
                store (state ! ix ~> R.state)        =<< unpack   buff  j'
                store (state ! ix ~> R.group)        =<< unpack   buff (j' + 1)
                store (state ! ix ~> R.delay)        =<< unpackLE buff (j' + 2)
                store (state ! ix ~> R.defaultDelay) =<< unpackLE buff (j' + 2)
                store (state ! ix ~> R.timestamp)    =<< getSystemTime clock
                store j $ j' + 6
        store (addrOf shouldInit) false
