{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Endpoint.Relays where

import           Control.Monad.Reader  (MonadReader, asks)
import           Control.Monad.Writer  (MonadWriter)
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
    ; timestampOn     :: Uint32
    ; timestampOff    :: Uint32
    ; group           :: Uint8
    ; synced          :: IBool
    }
|]



data Relays = Relays
    { runRelays :: RunRecords RelayStruct
    , payload   :: Buffer 8 Uint8
    , clock     :: SystemClock
    }

relays :: (MonadWriter Context m, MonadReader (D.Domain p0 t0) m) => String -> Int -> m Relays
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
                 , delayOff        .= ival 0
                 , delayOn         .= ival 0
                 , timestampOn     .= ival 0
                 , timestampOff    .= ival 0
                 , group           .= ival i
                 , synced          .= ival true
                 ]



message :: Relays -> Uint8 -> Ivory eff (Buffer 8 Uint8)
message Relays{..} i = do
    runRelays $ \r -> do
        let relay = addrOf r ! toIx i
        pack   payload 0 (0 :: Uint8)
        pack   payload 1 $ i + 1
        pack   payload 2 =<< deref (relay ~> state)
        pack   payload 3 =<< deref (relay ~> group)
        packLE payload 4 =<< deref (relay ~> defaultDelayOff)
    pure payload



turnOffRelay :: Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
turnOffRelay Relays{..} index = runRelays $ \rs -> do
    let ix     = index - 1
    let r      = addrOf rs ! ix
    shouldOff <- deref (r ~> state)
    when shouldOff $ do
        store (r ~> state       ) false
        store (r ~> delayOn     ) 0
        store (r ~> delayOff    ) 0
        store (r ~> timestampOff) =<< getSystemTime clock


turnOnRelay :: Relays -> G.Groups -> (forall n. KnownNat n => Ix n) -> Ivory ('Effects (Returns ()) r (Scope s)) ()
turnOnRelay Relays{..} groups index = runRelays $ \rs -> do
    let ix    = index - 1
    let rs'   = addrOf rs
    let r     = rs' ! ix
    shouldOn <- iNot <$> deref (r ~> state)
    when shouldOn $ do
        timestamp' <- getSystemTime clock
        group'     <- deref (r ~> group)
        turnOffGroup rs' ix group' timestamp'
        delayOn'   <- getGroupDelay rs' groups group' timestamp'
        store (r ~> state      ) $ delayOn' ==? 0
        store (r ~> delayOn    ) delayOn'
        store (r ~> delayOff   ) =<< deref (r ~> defaultDelayOff)
        store (r ~> timestampOn) timestamp'


turnOnRelay' :: Relays -> G.Groups -> (forall n. KnownNat n => Ix n) -> Uint32 -> Ivory ('Effects (Returns ()) r (Scope s)) ()
turnOnRelay' Relays{..} groups index delayOff' = runRelays $ \rs -> do
    let ix    = index - 1
    let rs'   = addrOf rs
    let r     = rs' ! ix
    shouldOn <- iNot <$> deref (r ~> state)
    when shouldOn $ do
        timestamp' <- getSystemTime clock
        group'     <- deref (r ~> group)
        turnOffGroup rs' ix group' timestamp'
        delayOn'   <- getGroupDelay rs' groups group' timestamp'
        store (r ~> state      ) $ delayOn' ==? 0
        store (r ~> delayOn    ) delayOn'
        store (r ~> delayOff   ) delayOff'
        store (r ~> timestampOn) timestamp'


toggleRelay :: Relays -> G.Groups -> (forall n. KnownNat n => Ix n) -> Ivory ('Effects (Returns ()) r (Scope s)) ()
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


setRelayGroup :: Relays -> Uint8 -> Uint8 -> Ivory (ProcEffects s ()) ()
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
              -> Ivory ('Effects (Returns ()) r (Scope s)) Uint32
getGroupDelay rs groups i ts = do
    delay' <- G.runGroups groups $ \gs -> do
        let g = addrOf gs ! toIx (i - 1)
        deref (g ~> G.delay)
    min <- local $ ival delay'
    arrayMap $ \jx -> do
        let r = rs ! jx
        group <- deref $ r ~> group
        when (group ==? i) $ do
            dt <- (ts -) <$> deref (r ~> timestampOff)
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
             -> Ivory ('Effects (Returns ()) r (Scope s)) ()
turnOffGroup rs ix g t =
    arrayMap $ \jx -> do
        when (jx /=? ix) $ do
            let r = rs ! jx
            group <- deref $ r ~> group
            when (group ==? g) $ do
                isOn  <- deref $ r ~> state
                delay <- deref $ r ~> delayOn
                when (isOn .|| delay >? 0) $ do
                    store (r ~> state       ) false
                    store (r ~> delayOn     ) 0
                    store (r ~> delayOff    ) 0
                    store (r ~> timestampOff) t
