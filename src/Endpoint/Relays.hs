{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Endpoint.Relays where

import           Control.Monad.Reader  (MonadReader, asks)
import           Control.Monad.State   (MonadState)
import           Core.Actions
import           Core.Context
import qualified Core.Domain           as D
import           Data.Buffer
import           Data.Record
import           Data.Serialize
import qualified Endpoint.Groups       as G
import           GHC.TypeNats
import           Interface.MCU
import           Interface.SystemClock (SystemClock, getSystemTime)
import           Ivory.Language
import           Ivory.Stdlib



type RelayStruct = "relay_struct"

[ivory|
    struct relay_struct
    { state           :: IBool
    ; defaultDelayOff :: Uint32
    ; delayOff        :: Uint32
    ; delayOn         :: Uint32
    ; timestamp       :: Uint32
    ; group           :: Uint8
    ; synced          :: IBool
    ; lock            :: IBool
    }
|]



data Relays = Relays
    { runRelays :: RunRecords RelayStruct
    , payload   :: Buffer 8 Uint8
    , clock     :: SystemClock
    }

relays :: (MonadState Context m, MonadReader (D.Domain p0 t0) m) => String -> Int -> m Relays
relays name n = do
    mcu          <- asks D.mcu
    let clock     = systemClock mcu
    addStruct (Proxy :: Proxy RelayStruct)
    let runRelays = runRecords name $ go . fromIntegral <$> [1..n]
    payload      <- buffer "relay_message"
    let relays    = Relays {runRelays, payload, clock}
    runRelays addArea
    pure relays
    where go i = [ state           .= ival false
                 , defaultDelayOff .= ival 0
                 , delayOn         .= ival 0
                 , delayOff        .= ival 0
                 , timestamp       .= ival 0
                 , group           .= ival i
                 , synced          .= ival false
                 , lock            .= ival false
                 ]



message :: Relays -> Uint8 -> Ivory eff (Buffer 8 Uint8)
message Relays{..} i = do
    runRelays $ \r -> do
        let relay = addrOf r ! toIx i
        pack   payload 0 actionDo
        pack   payload 1 $ i + 1
        pack   payload 2 =<< deref (relay ~> state)
        pack   payload 3 =<< deref (relay ~> group)
        packLE payload 4 =<< deref (relay ~> defaultDelayOff)
    pure payload



turnOffRelay :: Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
turnOffRelay Relays{..} index = runRelays $ \rs -> do
    let ix     = index - 1
    let r      = addrOf rs ! ix
    isLocked  <- deref $ r ~> lock
    shouldOff <- deref $ r ~> state
    when (iNot isLocked .&& shouldOff) $ do
        store (r ~> state    ) false
        store (r ~> delayOn  ) 0
        store (r ~> delayOff ) 0
        store (r ~> timestamp) =<< getSystemTime clock


turnOnRelay :: Relays -> G.Groups -> (forall n. KnownNat n => Ix n) -> Ivory ('Effects (Returns t) b (Scope s)) ()
turnOnRelay Relays{..} groups index = runRelays $ \rs -> do
    let ix    = index - 1
    let rs'   = addrOf rs
    let r     = rs' ! ix
    isLocked <- deref $ r ~> lock
    delayOn' <- deref $ r ~> delayOn
    isOn     <- deref $ r ~> state
    when (iNot isLocked .&& iNot isOn .&& delayOn' ==? 0) $ do
        timestamp' <- getSystemTime clock
        group'     <- deref $ r ~> group
        turnOffGroup rs' ix group' timestamp'
        delay' <- getGroupDelay rs' groups group' timestamp'
        store (r ~> delayOn) delay'
        store (r ~> timestamp) timestamp'
        when  (delay' ==? 0) $ store (r ~> state) true
    store (r ~> delayOff) =<< deref (r ~> defaultDelayOff)


turnOnRelay' :: Relays -> G.Groups -> (forall n. KnownNat n => Ix n) -> Uint32 -> Ivory ('Effects (Returns t) b (Scope s)) ()
turnOnRelay' Relays{..} groups index delayOff' = runRelays $ \rs -> do
    let ix    = index - 1
    let rs'   = addrOf rs
    let r     = rs' ! ix
    isLocked <- deref $ r ~> lock
    delayOn' <- deref $ r ~> delayOn
    isOn     <- deref $ r ~> state
    when (iNot isLocked .&& iNot isOn .&& delayOn' ==? 0) $ do
        timestamp' <- getSystemTime clock
        group'     <- deref $ r ~> group
        turnOffGroup rs' ix group' timestamp'
        delay' <- getGroupDelay rs' groups group' timestamp'
        store (r ~> delayOn) delay'
        store (r ~> timestamp) timestamp'
        when  (delay' ==? 0) $ store (r ~> state) true
    store (r ~> delayOff) delayOff'


toggleRelay :: Relays -> G.Groups -> (forall n. KnownNat n => Ix n) -> Ivory ('Effects (Returns t) b (Scope s)) ()
toggleRelay relays@Relays{..} groups index = runRelays $ \rs -> do
    let ix = index - 1
    let r  = addrOf rs ! ix
    state' <- deref $ r ~> state
    ifte_ state'
          (turnOffRelay relays index)
          (turnOnRelay  relays groups index)


setRelayDelayOff :: Relays -> Uint8 -> Uint32 -> Ivory eff ()
setRelayDelayOff Relays{..} index delay = runRelays $ \rs -> do
    let ix = toIx (index - 1)
    let r  = addrOf rs ! ix
    store (r ~> defaultDelayOff) delay
    store (r ~> synced         ) false


setRelayGroup :: Relays -> Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
setRelayGroup Relays{..} index group' = runRelays $ \rs -> do
    let ix  = toIx (index - 1)
    let rs' = addrOf rs
    let r   = rs' ! ix
    turnOffGroup rs' ix group' =<< getSystemTime clock
    store (r ~> group ) group'
    store (r ~> synced) false



getGroupDelay :: KnownNat n
              => Records n RelayStruct
              -> G.Groups
              -> Uint8
              -> Uint32
              -> Ivory ('Effects (Returns t) b (Scope s)) Uint32
getGroupDelay rs groups i ts = do
    delay' <- G.runGroups groups $ \gs -> do
        let g = addrOf gs ! toIx (i - 1)
        deref (g ~> G.delay)
    min <- local $ ival delay'
    arrayMap $ \jx -> do
        let r     = rs ! jx
        isLocked <- deref $ r ~> lock
        when (iNot isLocked) $ do
            isOn   <- deref $ r ~> state
            group' <- deref $ r ~> group
            when (iNot isOn .&& group' ==? i) $ do
                dt <- (ts -) <$> deref (r ~> timestamp)
                min' <- deref min
                when (dt <? min') $ store min dt
    min' <- deref min
    ifte (delay' >? min')
         (pure $ delay' - min')
         (pure 0)



turnOffGroup :: KnownNat n
             => Records n RelayStruct
             -> Ix n
             -> Uint8
             -> Uint32
             -> Ivory ('Effects (Returns t) b (Scope s)) ()
turnOffGroup rs ix g t =
    arrayMap $ \jx -> do
        when (jx /=? ix) $ do
            let r = rs ! jx
            group <- deref $ r ~> group
            when (group ==? g) $ do
                isOn     <- deref $ r ~> state
                delayOn' <- deref $ r ~> delayOn
                isLocked <- deref $ r ~> lock
                when (iNot isLocked) $
                    cond_ [ isOn ==> do
                                store (r ~> state    ) false
                                store (r ~> delayOn  ) 0
                                store (r ~> delayOff ) 0
                                store (r ~> timestamp) t
                        , delayOn' >? 0 ==> do
                                store (r ~> delayOn  ) 0
                                store (r ~> delayOff ) 0
                        ]
