{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Endpoint.ATS where

import           Control.Monad.Reader  (MonadReader, asks)
import           Control.Monad.State   (MonadState)
import           Core.Context
import           Core.Domain           (Domain (mcu))
import qualified Core.Domain           as D
import           Data.Buffer
import           Data.Record
import           Data.Value
import           Endpoint.DInputs      as DI
import           Endpoint.Relays       as R
import           Interface.MCU
import           Interface.SystemClock (SystemClock, getSystemTime)
import           Ivory.Language
import           Ivory.Stdlib



modeNone        = 0x00 :: Uint8
mode_N1_G       = 0x11 :: Uint8
mode_N2         = 0x20 :: Uint8
mode_N2_G       = 0x21 :: Uint8


stateOff        = 0x00 :: Uint8
stateOn         = 0x01 :: Uint8

stateTurningOff = 0x10 :: Uint8
stateTurningOn  = 0x11 :: Uint8

stateError      = 0xff :: Uint8

maxAttempt      = 3    :: Uint8


errorNone       = 0    :: Uint8




data ATS = ATS
    { mode            :: Value    Uint8
    , state           :: Value    Uint8
    , currentSource   :: Value    Uint8
    , selectedSource  :: Value    Uint8
    , relayStatus     :: Values 3 IBool
    , generatorStatus :: Value    IBool
    , attempt         :: Value    Uint8
    , timestamp       :: Value    Uint32
    , clock           :: SystemClock
    , payload         :: Buffer 2 Uint8
    }


mkATS :: (MonadState Context m, MonadReader (Domain p t) m) => m ATS
mkATS = do
    mcu             <- asks D.mcu
    let clock        = systemClock mcu
    mode            <- value  "ats_mode"             modeNone
    state           <- value  "ats_state"            stateOff
    currentSource   <- value  "ats_current_source"   0
    selectedSource  <- value  "ats_selected_source"  0
    relayStatus     <- values "ats_relay_status"     [true, true, true]
    generatorStatus <- value  "ats_generator_status" true
    attempt         <- value  "ats_attempt"          0
    timestamp       <- value  "ats_timestamp"        0
    payload         <- buffer "ats_payload"
    pure ATS { mode
             , state
             , currentSource
             , selectedSource
             , relayStatus
             , generatorStatus
             , attempt
             , timestamp
             , clock
             , payload
             }



message :: ATS -> Ivory eff (Buffer 2 Uint8)
message ATS{..} = do
    store (payload ! 0) 0x4
    store (payload ! 1) =<< deref mode
    pure payload



manageATS :: ATS -> DInputs -> Relays -> Ivory eff ()
manageATS a@ATS{..} DInputs{runDInputs} Relays{runRelays} =
    runDInputs $ \di -> runRelays $ \r -> do
        let di'  = addrOf di
        let r'   = addrOf r
        mode'   <- deref mode
        store selectedSource 0
        cond_ [ mode' ==? mode_N1_G ==> do manageLine      2 a (di' ! 0) (di' ! 1) (r' ! 0)
                                           manageGenerator 1 a (di' ! 2) (di' ! 3) (r' ! 1) (r' ! 2)
                                           manageReset       a (di' ! 4)

              , mode' ==? mode_N2   ==> do manageLine      2 a (di' ! 0) (di' ! 1) (r' ! 0)
                                           manageLine      1 a (di' ! 2) (di' ! 3) (r' ! 1)
                                           manageReset       a (di' ! 4)

              , mode' ==? mode_N2_G ==> do manageLine      3 a (di' ! 0) (di' ! 1) (r' ! 0)
                                           manageLine      2 a (di' ! 2) (di' ! 3) (r' ! 1)
                                           manageGenerator 1 a (di' ! 4) (di' ! 5) (r' ! 2) (r' ! 3)
                                           manageReset       a (di' ! 6)
              ]



manageLine :: Uint8
           -> ATS
           -> Record DInputStruct
           -> Record DInputStruct
           -> Record RelayStruct
           ->  Ivory eff ()
manageLine n ATS{..} hasVoltage isRelayOn relay = do
    state'    <- deref state
    timestamp <- getSystemTime clock
    ifte_ (state' ==? stateError)
          (turnOff relay timestamp)
          (do
                hasVoltage' <- deref $ hasVoltage ~> DI.state
                isRelayOn'  <- deref $ isRelayOn ~> DI.state
                relayState' <- deref $ relay ~> R.state
                ifte_ hasVoltage'
                    (do
                        selectedSource' <- deref selectedSource
                        ifte_ (n >? selectedSource')
                              (do
                                    turnOn' relay timestamp
                                    store selectedSource n
                              )
                              (turnOff relay timestamp)
                    )
                    (turnOff relay timestamp)
          )



manageGenerator :: Uint8
                -> ATS
                -> Record DInputStruct
                -> Record DInputStruct
                -> Record RelayStruct
                -> Record RelayStruct
                ->  Ivory eff ()
manageGenerator n ATS{..} hasVoltage isRelayOn relay start = do
    state'    <- deref state
    timestamp <- getSystemTime clock
    ifte_ (state' ==? stateError)
          (do
                turnOff relay timestamp
                turnOff start timestamp
          )
          (do
                isRelayOn'  <- deref $ isRelayOn ~> DI.state
                relayState' <- deref $ relay ~> R.state
                selectedSource' <- deref selectedSource
                ifte_ (n >? selectedSource')
                      (do
                            turnOn' start timestamp
                            store selectedSource n
                      )
                      (do
                            turnOff relay timestamp
                            turnOff start timestamp
                      )
          )



manageReset :: ATS
            -> Record DInputStruct
            ->  Ivory eff ()
manageReset ATS{..} reset = do
    state' <- deref state
    when (state' ==? stateError) $ do
        pure ()



turnOn :: Record RelayStruct -> Uint32 -> Ivory eff ()
turnOn relay timestamp = do
    isOn <- deref $ relay ~> R.state
    when (iNot isOn) $ do
        store (relay ~> R.state    ) true
        store (relay ~> R.delayOff ) 0
        store (relay ~> R.delayOn  ) 0
        store (relay ~> R.timestamp) timestamp


turnOn' :: Record RelayStruct -> Uint32 -> Ivory eff ()
turnOn' relay timestamp = do
    isOn    <- deref $ relay ~> R.state
    delayOn <- deref $ relay ~> R.delayOn
    when (iNot isOn .&& delayOn ==? 0) $ do
        store (relay ~> R.state    ) false
        store (relay ~> R.delayOff ) 0
        store (relay ~> R.delayOn  ) 1000
        store (relay ~> R.timestamp) timestamp


turnOff :: Record RelayStruct -> Uint32 -> Ivory eff ()
turnOff relay timestamp = do
    isOn <- deref $ relay ~> R.state
    when isOn $ do
        store (relay ~> R.state    ) false
        store (relay ~> R.delayOff ) 0
        store (relay ~> R.delayOn  ) 0
        store (relay ~> R.timestamp) timestamp
