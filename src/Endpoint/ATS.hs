{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Endpoint.ATS where

import           Control.Monad.Reader  (MonadReader, asks)
import           Control.Monad.State   (MonadState)
import           Core.Context
import           Core.Domain           (Domain (mcu))
import qualified Core.Domain           as D
import qualified Core.Transport        as T
import           Data.Buffer
import           Data.Record
import           Data.Value
import qualified Endpoint.DInputs      as DI
import qualified Endpoint.Relays       as R
import           GHC.TypeNats
import           Interface.MCU
import           Interface.SystemClock (SystemClock, getSystemTime)
import           Ivory.Language
import           Ivory.Stdlib



modeNone        = 0x00 :: Uint8
mode_N1_G       = 0x11 :: Uint8
mode_N2         = 0x20 :: Uint8
mode_N2_G       = 0x21 :: Uint8

stateOk         = 0x00 :: Uint8
stateError      = 0xff :: Uint8

maxAttempt      = 3    :: Uint8

srcNone         = 0xff :: Uint8


data ATS = ATS
    { mode            :: Value    Uint8
    , state           :: Value    Uint8
    , source          :: Value    Uint8
    , attempt         :: Value    Uint8
    , timestamp       :: Value    Uint32
    , reset           :: Value    IBool
    , clock           :: SystemClock
    , payload         :: Buffer 5 Uint8
    , synced          :: Value    IBool
    , transmit        :: forall n. KnownNat n
                      => Buffer n Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }


mkATS :: (MonadState Context m, MonadReader (Domain p t) m, T.Transport t) => m ATS
mkATS = do
    mcu             <- asks D.mcu
    transport       <- asks D.transport
    let clock        = systemClock mcu
    mode            <- value  "ats_mode"             modeNone
    state           <- value  "ats_state"            stateOk
    source          <- value  "ats_source"           srcNone
    attempt         <- value  "ats_attempt"          0
    timestamp       <- value  "ats_timestamp"        0
    synced          <- value  "ats_synced"           false
    reset           <- value  "ats_reset"            false
    payload         <- buffer "ats_payload"
    pure ATS { mode
             , state
             , source
             , attempt
             , timestamp
             , clock
             , reset
             , payload
             , synced
             , transmit = T.transmitBuffer transport
             }



message :: ATS -> Ivory eff (Buffer 5 Uint8)
message ATS{..} = do
    store (payload ! 0) 0x4
    store (payload ! 1) =<< deref mode
    store (payload ! 2) =<< deref source
    store (payload ! 3) =<< deref attempt
    store (payload ! 4) =<< deref state
    pure payload



manageATS :: ATS -> DI.DInputs -> R.Relays -> Ivory eff ()
manageATS a@ATS{..} DI.DInputs{runDInputs} R.Relays{runRelays} =
    runDInputs $ \di -> runRelays $ \r -> do
        let di'  = addrOf di
        let r'   = addrOf r
        mode'   <- deref mode
        cond_ [ mode' ==? mode_N1_G ==> do manageLine      1 a (di' ! 0) (di' ! 1) (r' ! 0)
                                           manageGenerator 2 a (di' ! 2) (di' ! 3) (r' ! 1) (r' ! 2)
                                           manageReset       a (di' ! 4)

              , mode' ==? mode_N2   ==> do manageLine      1 a (di' ! 0) (di' ! 1) (r' ! 0)
                                           manageLine      2 a (di' ! 2) (di' ! 3) (r' ! 1)
                                           manageReset       a (di' ! 4)

              , mode' ==? mode_N2_G ==> do manageLine      1 a (di' ! 0) (di' ! 1) (r' ! 0)
                                           manageLine      2 a (di' ! 2) (di' ! 3) (r' ! 1)
                                           manageGenerator 3 a (di' ! 4) (di' ! 5) (r' ! 2) (r' ! 3)
                                           manageReset       a (di' ! 6)
              ]



manageLine :: Uint8
           -> ATS
           -> Record DI.DInputStruct
           -> Record DI.DInputStruct
           -> Record R.RelayStruct
           ->  Ivory eff ()
manageLine n a@ATS{..} hasVoltage isRelayOn relay = do
    manageError n a isRelayOn relay

    timestamp <- getSystemTime clock
    state'    <- deref state
    ifte_ (state' ==? stateOk)
          (do
                hasVoltage' <- deref $ hasVoltage ~> DI.state
                source'     <- deref source
                ifte_ hasVoltage'
                    (do
                        cond_ [ n <? source' ==> do
                                    delayTurnOn relay 1000 timestamp
                                    store attempt 0
                                    store source n
                                    store synced false

                              , n >? source' ==> justTurnOff relay timestamp
                              ]
                    )
                    (do
                        justTurnOff relay timestamp
                        when (n ==? source') $ do
                            store source srcNone
                            store synced false
                    )
          )
          (justTurnOff relay timestamp)



manageGenerator :: Uint8
                -> ATS
                -> Record DI.DInputStruct
                -> Record DI.DInputStruct
                -> Record R.RelayStruct
                -> Record R.RelayStruct
                ->  Ivory eff ()
manageGenerator n a@ATS{..} hasVoltage isRelayOn relay start = do
    manageError n a isRelayOn relay

    timestamp <- getSystemTime clock
    state'    <- deref state
    ifte_ (state' ==? stateOk)
          (do
                hasVoltage' <- deref $ hasVoltage ~> DI.state
                isStarted'  <- deref $ start ~> R.state
                attempt'    <- deref attempt
                source'     <- deref source
                cond_ [ n <? source' ==> do
                            justTurnOff relay timestamp
                            delayTurnOn start 1000 timestamp
                            store attempt $ attempt' + 1
                            store source n
                            store synced false

                       , n ==? source' ==> do
                            ifte_ isStarted'
                                (
                                    ifte_ hasVoltage'
                                        (delayTurnOn relay 5000 timestamp)
                                        (do
                                            t <- deref $ start ~> R.timestamp
                                            when (timestamp - t >? 30000) $ do
                                                justTurnOff relay timestamp
                                                justTurnOff start timestamp
                                                when (attempt' <? maxAttempt) $ do
                                                    delayTurnOn start 10000 timestamp
                                                    store attempt $ attempt' + 1
                                                    store synced false
                                        )
                                )
                                (do
                                    justTurnOff relay timestamp
                                    delayTurnOn start 1000 timestamp
                                )

                       , true ==> do
                            justTurnOff relay timestamp
                            delayTurnOff start 20000 timestamp
                       ]

          )
          (do
                justTurnOff relay timestamp
                delayTurnOff start 20000 timestamp
          )



manageError :: Uint8
            -> ATS
            -> Record DI.DInputStruct
            -> Record R.RelayStruct
            ->  Ivory eff ()
manageError n ATS{..} isRelayOn relay = do
    delayOn'  <- deref $ relay ~> R.delayOn
    delayOff' <- deref $ relay ~> R.delayOff
    when (delayOn' ==? 0 .&& delayOff' ==? 0) $ do
        t0 <- deref $ relay ~> R.timestamp
        t1 <- getSystemTime clock
        when (t1 - t0 >? 100) $ do
            isRelayOn'  <- deref $ isRelayOn ~> DI.state
            relayState' <- deref $ relay ~> R.state
            when (relayState' /=? isRelayOn') $ do
                store state $ 100 * n + 10 * safeCast relayState' + safeCast isRelayOn'
                store synced false



manageReset :: ATS
            -> Record DI.DInputStruct
            ->  Ivory eff ()
manageReset ATS{..} di = do
    isPressed   <- deref $ di ~> DI.state
    shouldReset <- deref reset
    when (iNot isPressed .&& shouldReset) $ do
        store state stateOk
        store source srcNone
        store attempt 0
        store synced false
    store reset isPressed



delayTurnOn :: Record R.RelayStruct -> Uint32 -> Uint32 -> Ivory eff ()
delayTurnOn relay delay timestamp = do
    isOn    <- deref $ relay ~> R.state
    delayOn <- deref $ relay ~> R.delayOn
    when (iNot isOn .&& delayOn ==? 0) $ do
        store (relay ~> R.delayOff ) 0
        store (relay ~> R.delayOn  ) delay
        store (relay ~> R.timestamp) timestamp



justTurnOff :: Record R.RelayStruct -> Uint32 -> Ivory eff ()
justTurnOff relay timestamp = do
    isOn <- deref $ relay ~> R.state
    when isOn $ store (relay ~> R.timestamp) timestamp
    store (relay ~> R.state    ) false
    store (relay ~> R.delayOff ) 0
    store (relay ~> R.delayOn  ) 0



delayTurnOff :: Record R.RelayStruct -> Uint32 -> Uint32 -> Ivory eff ()
delayTurnOff relay delay timestamp = do
    isOn     <- deref $ relay ~> R.state
    delayOff <- deref $ relay ~> R.delayOff
    when (isOn .&& delayOff ==? 0) $ do
        store (relay ~> R.delayOff ) delay
        store (relay ~> R.delayOn  ) 0
        store (relay ~> R.timestamp) timestamp



syncATS :: ATS -> Ivory (ProcEffects s ()) ()
syncATS ats = do
    synced' <- deref $ synced ats
    when (iNot synced') $ do
        transmit ats =<< message ats
        store (synced ats) true
