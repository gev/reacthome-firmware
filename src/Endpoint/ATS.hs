{-# LANGUAGE RecordWildCards #-}

module Endpoint.ATS where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import Core.Domain (Domain (mcu))
import qualified Core.Domain as D
import qualified Core.Transport as T
import Data.Buffer
import Data.Record
import Data.Value
import qualified Endpoint.DInputs as DI
import Endpoint.Relays (delayOn)
import qualified Endpoint.Relays as R
import GHC.TypeNats
import Interface.MCU
import Interface.SystemClock (SystemClock, getSystemTime)
import Ivory.Language
import Ivory.Language.Uint (Uint8 (Uint8))
import Ivory.Stdlib

modeNone = 0x00 :: Uint8
mode_N1_G = 0x11 :: Uint8
mode_N2 = 0x20 :: Uint8
mode_N2_G = 0x21 :: Uint8

errorNone = 0x00 :: Uint8
errorGenerator = 0x01 :: Uint8

maxAttempt = 3 :: Uint8

srcNone = 0xff :: Uint8

data ATS = ATS
    { mode :: Value Uint8
    , error :: Values 4 Uint8
    , source :: Value Uint8
    , attempt :: Value Uint8
    , timestamp :: Value Uint32
    , reset :: Value IBool
    , clock :: SystemClock
    , payload :: Buffer 8 Uint8
    , synced :: Value IBool
    , transmit ::
        forall n.
        (KnownNat n) =>
        Buffer n Uint8 ->
        forall s.
        Ivory (ProcEffects s ()) ()
    }

mkATS :: (MonadState Context m, MonadReader (Domain p i) m, T.Transport t) => t -> m ATS
mkATS transport = do
    mcu <- asks D.mcu
    let clock = systemClock mcu
    mode <- value "ats_mode" modeNone
    error <- values "ats_error" [errorNone, errorNone, errorNone, errorNone]
    source <- value "ats_source" srcNone
    attempt <- value "ats_attempt" 0
    timestamp <- value "ats_timestamp" 0
    synced <- value "ats_synced" false
    reset <- value "ats_reset" false
    payload <- buffer "ats_payload"
    pure
        ATS
            { mode
            , error
            , source
            , attempt
            , timestamp
            , clock
            , reset
            , payload
            , synced
            , transmit = T.transmitBuffer transport
            }

message :: ATS -> Ivory eff (Buffer 8 Uint8)
message ATS{..} = do
    store (payload ! 0) actionMix
    store (payload ! 1) =<< deref mode
    store (payload ! 2) =<< deref source
    store (payload ! 3) =<< deref attempt
    store (payload ! 4) =<< deref (error ! 0)
    store (payload ! 5) =<< deref (error ! 1)
    store (payload ! 6) =<< deref (error ! 2)
    store (payload ! 7) =<< deref (error ! 3)
    pure payload

forceSyncATS :: ATS -> Ivory eff ()
forceSyncATS ATS{..} = store synced false

manageATS ::
    (KnownNat ni, KnownNat no) =>
    ATS ->
    DI.DInputs ni ->
    R.Relays no ->
    Ivory ('Effects (Returns ()) r (Scope s)) ()
manageATS a@ATS{..} DI.DInputs{dinputs} R.Relays{relays} = do
    mode' <- deref mode
    cond_
        [ mode' ==? mode_N1_G ==> do
            manageLine 1 a (dinputs ! 1) (dinputs ! 2) (relays ! 0)
            manageGenerator 2 a (dinputs ! 10) (dinputs ! 11) (relays ! 4) (relays ! 5)
            manageError a (dinputs ! 0)
        , mode' ==? mode_N2 ==> do
            manageLine 1 a (dinputs ! 1) (dinputs ! 2) (relays ! 0)
            manageLine 2 a (dinputs ! 3) (dinputs ! 4) (relays ! 1)
            manageError a (dinputs ! 0)
        , mode' ==? mode_N2_G ==> do
            manageLine 1 a (dinputs ! 1) (dinputs ! 2) (relays ! 0)
            manageLine 2 a (dinputs ! 3) (dinputs ! 4) (relays ! 1)
            manageGenerator 3 a (dinputs ! 10) (dinputs ! 11) (relays ! 4) (relays ! 5)
            manageError a (dinputs ! 0)
        ]

manageLine ::
    Uint8 ->
    ATS ->
    Record DI.DInputStruct ->
    Record DI.DInputStruct ->
    Record R.RelayStruct ->
    Ivory ('Effects (Returns ()) r (Scope s)) ()
manageLine n a@ATS{..} hasVoltage isRelayOn relay = do
    detectError n a hasVoltage isRelayOn relay
    timestamp <- getSystemTime clock
    noError <- iNot <$> hasError a
    ifte_
        noError
        ( do
            hasVoltage' <- deref $ hasVoltage ~> DI.state
            source' <- deref source
            ifte_
                hasVoltage'
                ( do
                    cond_
                        [ n <? source' ==> do
                            delayTurnOn relay 1_000 timestamp
                            store source n
                            store synced false
                        , n >? source' ==> justTurnOff relay timestamp
                        ]
                )
                ( do
                    justTurnOff relay timestamp
                    when (n ==? source') $ do
                        store source srcNone
                        store synced false
                )
        )
        (justTurnOff relay timestamp)

manageGenerator ::
    Uint8 ->
    ATS ->
    Record DI.DInputStruct ->
    Record DI.DInputStruct ->
    Record R.RelayStruct ->
    Record R.RelayStruct ->
    Ivory ('Effects (Returns ()) r (Scope s)) ()
manageGenerator n a@ATS{..} hasVoltage isRelayOn relay start = do
    detectError n a hasVoltage isRelayOn relay
    timestamp <- getSystemTime clock
    noError <- iNot <$> hasError a
    ts <- deref $ start ~> R.timestamp
    tr <- deref $ relay ~> R.timestamp
    ifte_
        noError
        ( do
            hasVoltage' <- deref $ hasVoltage ~> DI.state
            isStarted' <- deref $ start ~> R.state
            attempt' <- deref attempt
            source' <- deref source
            cond_
                [ n <? source' ==> do
                    justTurnOff relay timestamp
                    when (iNot isStarted') $
                        store (start ~> R.timestamp) timestamp
                    store source n
                    store synced false
                , n ==? source' ==> do
                    ifte_
                        isStarted'
                        ( do
                            isOn <- deref $ relay ~> R.state
                            when isOn $ do
                                store (start ~> R.timestamp) timestamp
                                when (attempt' /=? 0 .&& timestamp - tr >? 60_000) $ do
                                    store attempt 0
                                    store synced false
                            ifte_
                                hasVoltage'
                                (delayTurnOn relay 5_000 timestamp)
                                ( do
                                    when (timestamp - ts >? 30_000) $ justTurnOff start timestamp
                                    justTurnOff relay timestamp
                                )
                        )
                        ( do
                            justTurnOff relay timestamp
                            error' <- deref $ error ! 0
                            when (error' ==? errorNone) $ do
                                t <- deref $ start ~> R.timestamp
                                when (timestamp - t >? 10_000) $
                                    cond_
                                        [ attempt' ==? 0 ==> do
                                            justTurnOn start timestamp
                                            store attempt 1
                                            store synced false
                                        , attempt' <? maxAttempt ==> do
                                            justTurnOn start timestamp
                                            store attempt $ attempt' + 1
                                            store synced false
                                        , true ==> do
                                            store (error ! 0) errorGenerator
                                            store synced false
                                        ]
                        )
                , true ==> do
                    attempt' <- deref attempt
                    error' <- deref $ error ! 0
                    when (error' ==? errorNone .&& attempt' /=? 0 .&& iNot isStarted' .&& timestamp - ts >? 60_000) $ do
                        store attempt 0
                        store synced false
                    justTurnOff relay timestamp
                    when (timestamp - ts >? 30_000) $ justTurnOff start timestamp
                ]
        )
        ( do
            justTurnOff relay timestamp
            when (timestamp - ts >? 30_000) $ justTurnOff start timestamp
        )

hasError :: ATS -> Ivory eff IBool
hasError ATS{..} = do
    (.||) <$> ((.||) <$> e 1 <*> e 2) <*> e 3
  where
    e i = (/=? errorNone) <$> deref (error ! i)

detectError ::
    Uint8 ->
    ATS ->
    Record DI.DInputStruct ->
    Record DI.DInputStruct ->
    Record R.RelayStruct ->
    Ivory ('Effects (Returns ()) r (Scope s)) ()
detectError n ATS{..} hasVoltage isRelayOn relay = do
    delayOn' <- deref $ relay ~> R.delayOn
    delayOff' <- deref $ relay ~> R.delayOff
    error' <- deref $ error ! toIx n
    when (error' ==? errorNone .&& delayOn' ==? 0 .&& delayOff' ==? 0) $ do
        t1 <- deref $ hasVoltage ~> DI.timestamp
        t2 <- deref $ isRelayOn ~> DI.timestamp
        t3 <- deref $ relay ~> R.timestamp
        t <- getSystemTime clock
        let dt1 = t - t1
        let dt2 = t - t2
        let dt3 = t - t3
        dt <- ifte (dt1 <? dt2) (pure dt1) (pure dt2)
        dt <- ifte (dt <? dt3) (pure dt) (pure dt3)
        when (dt >? 1_000) $ do
            hasVoltage' <- deref $ hasVoltage ~> DI.state
            isRelayOn' <- deref $ isRelayOn ~> DI.state
            relayState' <- deref $ relay ~> R.state
            cond_
                [ iNot hasVoltage' .&& iNot relayState' .&& isRelayOn' ==> err 1 -- 0 0 1
                , iNot hasVoltage' .&& relayState' .&& iNot isRelayOn' ==> err 2 -- 0 1 0
                , iNot hasVoltage' .&& relayState' .&& isRelayOn' ==> err 3 -- 0 1 1
                , hasVoltage' .&& iNot relayState' .&& isRelayOn' ==> err 5 -- 1 0 1
                , hasVoltage' .&& relayState' .&& iNot isRelayOn' ==> err 6 -- 1 1 0
                ]
  where
    err code = do
        store (error ! toIx n) code
        store synced false

manageError ::
    ATS ->
    Record DI.DInputStruct ->
    Ivory eff ()
manageError a@ATS{..} di = do
    isPressed <- deref $ di ~> DI.state
    shouldReset <- deref reset
    when (iNot isPressed .&& shouldReset) $ do
        resetError a
    store reset isPressed

resetError :: ATS -> Ivory eff ()
resetError ATS{..} = do
    store (error ! 0) errorNone
    store (error ! 1) errorNone
    store (error ! 2) errorNone
    store (error ! 3) errorNone
    store source srcNone
    store attempt 0
    store synced false

justTurnOn :: Record R.RelayStruct -> Uint32 -> Ivory eff ()
justTurnOn relay timestamp = do
    isOn <- deref $ relay ~> R.state
    when (iNot isOn) $ store (relay ~> R.timestamp) timestamp
    store (relay ~> R.state) true
    store (relay ~> R.delayOff) 0
    store (relay ~> R.delayOn) 0

delayTurnOn :: Record R.RelayStruct -> Uint32 -> Uint32 -> Ivory eff ()
delayTurnOn relay delay timestamp = do
    isOn <- deref $ relay ~> R.state
    delayOn <- deref $ relay ~> R.delayOn
    when (iNot isOn) $ do
        store (relay ~> R.delayOn) delay
        when (delayOn ==? 0) $ store (relay ~> R.timestamp) timestamp
    store (relay ~> R.delayOff) 0

justTurnOff :: Record R.RelayStruct -> Uint32 -> Ivory eff ()
justTurnOff relay timestamp = do
    isOn <- deref $ relay ~> R.state
    when isOn $ store (relay ~> R.timestamp) timestamp
    store (relay ~> R.state) false
    store (relay ~> R.delayOff) 0
    store (relay ~> R.delayOn) 0

delayTurnOff :: Record R.RelayStruct -> Uint32 -> Uint32 -> Ivory eff ()
delayTurnOff relay delay timestamp = do
    isOn <- deref $ relay ~> R.state
    delayOff <- deref $ relay ~> R.delayOff
    when isOn $ do
        store (relay ~> R.delayOff) delay
        when (delayOff ==? 0) $ store (relay ~> R.timestamp) timestamp
    store (relay ~> R.delayOn) 0

syncATS :: ATS -> Ivory (ProcEffects s ()) ()
syncATS ats = do
    synced' <- deref $ synced ats
    when (iNot synced') $ do
        transmit ats =<< message ats
        store (synced ats) true
