{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.Relays where

import           Control.Monad         (zipWithM_)
import           Control.Monad.Reader  (MonadReader, asks)
import           Control.Monad.Writer  (MonadWriter)
import           Core.Context
import           Core.Controller
import qualified Core.Domain           as D
import           Core.Feature
import           Core.Task
import qualified Core.Transport        as T
import           Data.Buffer
import           Data.Index
import           Data.Record
import           Data.Serialize
import           Data.Value
import qualified Endpoint.Groups       as G
import qualified Endpoint.Relays       as R
import           GHC.TypeNats
import           Interface.GPIO.Output
import           Interface.MCU         (MCU, peripherals, systemClock)
import           Interface.SystemClock (SystemClock, getSystemTime)
import           Ivory.Language
import           Ivory.Stdlib



data Relays = forall o. Output o => Relays
    { n           :: Uint8
    , getRelays   :: R.Relays
    , getGroups   :: G.Groups
    , getOutputs  :: [o]
    , shouldInit  :: Value IBool
    , clock       :: SystemClock
    , current     :: Index Uint8
    , transmit    :: forall n. KnownNat n
                  => Buffer n Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }



mkRelays :: (MonadWriter Context m, MonadReader (D.Domain p t) m, T.Transport t, Output o)
         => [p -> m o] -> m Relays
mkRelays outs = do
    mcu        <- asks D.mcu
    transport  <- asks D.transport
    shouldInit <- asks D.shouldInit
    os         <- mapM ($ peripherals mcu) outs
    let n       = length os
    getRelays  <- R.relays "relays" n
    getGroups  <- G.groups "groups" n
    current    <- index "current_relay"
    let clock   = systemClock mcu
    pure Relays { n = fromIntegral n
                , getRelays
                , getGroups
                , getOutputs = os
                , shouldInit
                , clock
                , current
                , transmit = T.transmitBuffer transport
                }



relays :: (MonadWriter Context m, MonadReader (D.Domain p t) m, T.Transport t, Output o)
       => [p -> m o] -> m Feature
relays outs = do
    relays <- mkRelays outs
    addTask $ delay 10 "relays_manage" $ manageRelays relays
    addTask $ yeld     "relays_sync"   $ syncRelays   relays
    pure    $ Feature relays



manageRelays :: Relays -> Ivory eff ()
manageRelays Relays{..} = zipWithM_ zip getOutputs [0..]
    where
        zip :: Output o => o -> Int -> Ivory eff ()
        zip output i = R.runRelays getRelays $ \rs -> do
            let ix = fromIntegral i
            let r = addrOf rs ! ix
            let runState = manageState r output
            let runDelay = manageDelay r $ getSystemTime clock
            isOn <- deref $ r ~> R.state
            ifte_ isOn
                (do
                    runState set        true
                    runDelay R.delayOff false
                )
                (do
                    runState reset      false
                    runDelay R.delayOn  true
                )



manageState :: Output o
            => Record R.RelayStruct
            -> o
            -> (o -> Ivory eff ())
            -> IBool
            -> Ivory eff ()
manageState r o setOut state = do
    state' <- get o
    when (state' /=? state) $ do
        store (r ~> R.synced) false
        setOut o


manageDelay :: Record R.RelayStruct
            -> Ivory eff Uint32
            -> Label R.RelayStruct ('Stored Uint32)
            -> IBool
            -> Ivory eff ()
manageDelay r timestamp delay state = do
    delay' <- deref $ r ~> delay
    when (delay' >? 0) $ do
        t0 <- deref $ r ~> R.timestamp
        t1 <- timestamp
        when (t1 - t0 >=? delay') $ do
            store (r ~> delay      ) 0
            store (r ~> R.state    ) state
            store (r ~> R.timestamp) t1



syncRelays :: Relays -> Ivory (ProcEffects s ()) ()
syncRelays rs@Relays{..} = do
    i <- deref current
    syncRelays' rs i
    syncGroups' rs i
    store current $ i + 1



syncRelays' :: Relays -> Uint8 -> Ivory (ProcEffects s ()) ()
syncRelays' Relays{..} i =
    R.runRelays getRelays $ \rs -> do
        let r = addrOf rs ! toIx i
        synced <- deref $ r ~> R.synced
        when (iNot synced) $ do
            msg <- R.message getRelays (i .% n)
            transmit msg
            store (r ~> R.synced) true



syncGroups' :: Relays -> Uint8 -> Ivory (ProcEffects s ()) ()
syncGroups' Relays{..} i =
    G.runGroups getGroups $ \gs -> do
        let g = addrOf gs ! toIx i
        synced <- deref $ g ~> G.synced
        when (iNot synced) $ do
            msg <- G.message getGroups (i .% n)
            transmit msg
            store (g ~> G.synced) true



instance Controller Relays where
    handle rs buff size = do
        shouldInit' <- deref $ shouldInit rs
        pure [ size >=? 3 ==> do
                action <- deref $ buff ! 0
                cond_ [ iNot shouldInit' ==> cond_
                      [ action ==? 0x00  ==> onDo    rs buff size
                      , action ==? 0x02  ==> onGroup rs buff size
                      ]
                      , action ==? 0xf2  ==> onInit  rs buff size
                      ]
             ]



onDo :: KnownNat n
     => Relays
     -> Buffer n Uint8
     -> Uint8
     -> Ivory (ProcEffects s ()) ()
onDo relays@Relays{..} buff size = do
    index <- deref $ buff ! 1
    when (size >=? 3 .&& index >=? 1 .&& index <=? n) $ do
        action <- deref $ buff ! 2
        cond_ [ action ==? 0 ==> turnOffRelay relays (toIx index)
              , action ==? 1 ==> do
                    cond_ [ size >=? 7 ==> turnOnRelay' relays (toIx index) =<< unpackLE buff 3
                          , true       ==> turnOnRelay  relays (toIx index)
                          ]
              , action ==? 2 .&& size >=? 7 ==> setRelayDelayOff relays index =<< unpackLE buff 3
              , action ==? 3 ==> setRelayGroup relays index =<< unpack buff 3
              ]


turnOffRelay :: Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
turnOffRelay Relays{..} index = R.runRelays getRelays $ \rs -> do
    let ix = index - 1
    let r  = addrOf rs ! ix
    state' <- deref $ r ~> R.state
    when state' $ do
        store (r ~> R.state    ) false
        store (r ~> R.timestamp) =<< getSystemTime clock


turnOnRelay :: Relays -> (forall n. KnownNat n => Ix n) -> Ivory ('Effects (Returns ()) r (Scope s)) ()
turnOnRelay Relays{..} index = R.runRelays getRelays $ \rs -> do
    let ix = index - 1
    let r  = addrOf rs ! ix
    state' <- deref $ r ~> R.state
    when (iNot state') $ do
        shouldDelay <- syncGroup getGroups rs ix
        when  (iNot shouldDelay) $ store (r ~> R.state) true
        store (r ~> R.delayOff ) =<< deref (r ~> R.defaultDelayOff)
        store (r ~> R.timestamp) =<< getSystemTime clock


turnOnRelay' :: Relays -> (forall n. KnownNat n => Ix n) -> Uint32 -> Ivory ('Effects (Returns ()) r (Scope s)) ()
turnOnRelay' Relays{..} index delay = R.runRelays getRelays $ \rs -> do
    let ix = index - 1
    let r  = addrOf rs ! ix
    state' <- deref $ r ~> R.state
    when (iNot state') $ do
        shouldDelay <- syncGroup getGroups rs ix
        when  (iNot shouldDelay) $ store (r ~> R.state) true
        store (r ~> R.delayOff ) delay
        store (r ~> R.timestamp) =<< getSystemTime clock


toggleRelay :: Relays -> (forall n. KnownNat n => Ix n) -> Ivory ('Effects (Returns ()) r (Scope s)) ()
toggleRelay relays@Relays{..} index = R.runRelays getRelays $ \rs -> do
    let ix = index - 1
    let r  = addrOf rs ! ix
    state' <- deref $ r ~> R.state
    ifte_ state'
          (turnOffRelay relays index)
          (turnOnRelay  relays index)


setRelayDelayOff :: Relays -> Uint8 -> Uint32 -> Ivory eff ()
setRelayDelayOff Relays{..} index delay = R.runRelays getRelays $ \rs -> do
    let ix = toIx (index - 1)
    let r  = addrOf rs ! ix
    store (r ~> R.defaultDelayOff) delay
    store (r ~> R.synced         ) false


setRelayGroup :: Relays -> Uint8 -> Uint8 -> Ivory (ProcEffects s ()) ()
setRelayGroup Relays{..} index group = R.runRelays getRelays $ \rs -> do
    let ix = toIx (index - 1)
    let r  = addrOf rs ! ix
    turnOffGroup rs ix group
    store (r ~> R.group ) group
    store (r ~> R.synced) false



syncGroup :: KnownNat n
          => G.Groups
          -> Records' n R.RelayStruct
          -> Ix n
          -> Ivory ('Effects (Returns ()) r (Scope s)) IBool
syncGroup groups rs ix = do
    let r = addrOf rs ! ix
    group <- deref $ r ~> R.group
    shouldDelay <- isTurnOffGroup rs ix group
    when shouldDelay $
        G.runGroups groups $ \gs -> do
            let g = addrOf gs ! toIx (group - 1)
            delay <- deref $ g ~> G.delay
            store (r ~> R.delayOn) (delay + 1)
    pure shouldDelay


isTurnOffGroup :: KnownNat n
             => Records' n R.RelayStruct
             -> Ix n
             -> Uint8
             -> Ivory ('Effects (Returns ()) r (Scope s)) IBool
isTurnOffGroup rs ix g = do
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

turnOffGroup :: KnownNat n
             => Records' n R.RelayStruct
             -> Ix n
             -> Uint8
             -> Ivory (ProcEffects s ()) ()
turnOffGroup rs ix g =
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


onGroup :: KnownNat n
        => Relays
        -> Buffer n Uint8
        -> Uint8
        -> Ivory eff ()
onGroup Relays{..} buff size =
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
onInit :: KnownNat n
        => Relays
        -> Buffer n Uint8
        -> Uint8
        -> Ivory (ProcEffects s ()) ()
onInit rs@Relays{..} buff size =
    when (size >=? 1 + 5 * n + 6 * n) $ do
        offset <- local $ ival 1
        initGroups rs buff offset
        initRelays rs buff offset
        store shouldInit false



initGroups :: KnownNat n
           => Relays
           -> Buffer n Uint8
           -> Ref s (Stored (Ix n))
           -> Ivory eff ()
initGroups Relays{..} buff offset =
    G.runGroups getGroups $ \gs ->
        arrayMap $ \ix -> do
            offset' <- deref offset
            let g = addrOf gs ! ix
            store (g ~> G.enabled) =<< unpack   buff  offset'
            store (g ~> G.delay  ) =<< unpackLE buff (offset' + 1)
            store offset $ offset' + 5



initRelays :: KnownNat n
           => Relays
           -> Buffer n Uint8
           -> Ref s (Stored (Ix n))
           -> Ivory eff ()
initRelays Relays{..} buff offset =
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
