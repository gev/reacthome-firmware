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
import           Data.Record
import           Data.Serialize
import           Data.Value
import qualified Endpoint.Groups         as G
import qualified Endpoint.Relays         as R
import           GHC.TypeNats
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
               , delay  5 "relays_sync"   $ sync rs
               ]



manage :: Relays -> Ivory eff ()
manage (Relays {getRelays, getGroups, getOutputs, clock}) = do
    R.runRelays getRelays $ \rs -> do
        arrayMap $ \ix -> do
            let r = addrOf rs ! ix
            let run = manageRelay r getOutputs ix $ getSystemTime clock
            isOn <- deref $ r ~> R.state
            ifte_ isOn
                (run I.set   R.delayOff)
                (run I.reset R.delayOn )



manageRelay :: (KnownNat n, Outputs os)
            => Ref Global (Struct R.RelayStruct)
            -> os
            -> Ix n
            -> Ivory eff Uint32
            -> (os -> (forall n. KnownNat n => Ix n) -> Ivory eff ())
            -> Label R.RelayStruct ('Stored Uint32)
            -> Ivory eff ()
manageRelay r o ix timestamp setOut delay = do
    setOut o $ toIx $ fromIx ix
    delay' <- deref $ r ~> delay
    when (delay' >? 0) $ do
        t0 <- deref $ r ~> R.timestamp
        t1 <- timestamp
        when (t1 >=? t0 + delay') $ do
            store (r ~> delay      ) 0
            store (r ~> R.state    ) true
            store (r ~> R.timestamp) t1
            store (r ~> R.synced   ) false



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
        let r = addrOf rs ! toIx i
        synced <- deref $ r ~> R.synced
        when (iNot synced) $ do
            transmit =<< R.message getRelays (i .% n)
            store (r ~> R.synced) true



syncGroups :: Relays -> Uint8 -> Ivory (ProcEffects s ()) ()
syncGroups (Relays {n, getGroups, transmit}) i =
    G.runGroups getGroups $ \gs -> do
        let g = addrOf gs ! toIx i
        synced <- deref $ g ~> G.synced
        when (iNot synced) $ do
            transmit =<< G.message getGroups (i .% n)
            store (g ~> G.synced) true



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
            let ix = toIx (index - 1)
            let r  = addrOf rs ! ix
            action <- deref $ buff ! 2
            cond_ [ action ==? 0 ==> do
                        store (r ~> R.state )  false
                        store (r ~> R.synced) false
                  , action ==? 1 ==> do
                        store (r ~> R.timestamp) =<< getSystemTime clock
                        shouldDelay <- syncGroup getGroups rs ix
                        cond_ [ size >=? 7 ==> do store (r ~> R.delayOff) =<< unpackLE buff 3
                              , true       ==> do store (r ~> R.delayOff) =<< deref (r ~> R.defaultDelayOff)
                              ]
                        when (iNot shouldDelay) $ do
                            store (r ~> R.state ) true
                            store (r ~> R.synced) false
                  , action ==? 2 .&& size >=? 7 ==> do
                        store (r ~> R.defaultDelayOff) =<< unpackLE buff 3
                        store (r ~> R.synced         ) false
                  , action ==? 3 ==> do
                        group <- unpack buff 3
                        turnOffGroup rs ix group
                        store (r ~> R.group ) group
                        store (r ~> R.synced) false
                  ]



syncGroup :: KnownNat n
          => G.Groups
          -> Records n R.RelayStruct
          -> Ix n
          -> Ivory (ProcEffects s ()) IBool
syncGroup groups rs ix = do
    let r = addrOf rs ! ix
    group <- deref $ r ~> R.group
    shouldDelay <- turnOffGroup rs ix group
    when shouldDelay $
        G.runGroups groups $ \gs -> do
            let g = addrOf gs ! toIx (group - 1)
            delay <- deref $ g ~> G.delay
            store (r ~> R.delayOn) (delay + 1)
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
            let r = addrOf rs ! jx
            group <- deref $ r ~> R.group
            when (group ==? g) $ do
                isOn  <- deref $ r ~> R.state
                delay <- deref $ r ~> R.delayOn
                when (isOn .|| delay >? 0) $ do
                    store (r ~> R.state   ) false
                    store (r ~> R.delayOn ) 0
                    store (r ~> R.synced  ) false
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
                let g = addrOf gs ! toIx (index - 1)
                store (g ~> G.enabled) =<< unpack   buff 2
                store (g ~> G.delay  ) =<< unpackLE buff 3
                store (g ~> G.synced ) false



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
        offset <- local $ ival 1
        initGroups rs buff offset
        initRelays rs buff offset
        store (addrOf shouldInit) false



initGroups :: KnownNat n
           => Relays
           -> Ref Global (Array n ('Stored Uint8))
           -> Ref s (Stored (Ix n))
           -> Ivory eff ()
initGroups (Relays {getGroups}) buff offset =
    G.runGroups getGroups $ \gs ->
        arrayMap $ \ix -> do
            offset' <- deref offset
            let g = addrOf gs ! ix
            store (g ~> G.enabled) =<< unpack   buff  offset'
            store (g ~> G.delay  ) =<< unpackLE buff (offset' + 1)
            store offset $ offset' + 5



initRelays :: KnownNat n
           => Relays
           -> Ref Global (Array n ('Stored Uint8))
           -> Ref s (Stored (Ix n))
           -> Ivory eff ()
initRelays (Relays {clock, getRelays}) buff offset =
    R.runRelays getRelays $ \rs ->
        arrayMap $ \ix -> do
            offset' <- deref offset
            let r = addrOf rs ! ix
            store (r ~> R.state          ) =<< unpack   buff  offset'
            store (r ~> R.group          ) =<< unpack   buff (offset' + 1)
            store (r ~> R.delayOff       ) =<< unpackLE buff (offset' + 2)
            store (r ~> R.defaultDelayOff) =<< unpackLE buff (offset' + 2)
            store (r ~> R.timestamp      ) =<< getSystemTime clock
            store offset $ offset' + 6
