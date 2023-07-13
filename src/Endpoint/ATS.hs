{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

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
import           Ivory.Language.Uint   (Uint8 (Uint8))
import           Ivory.Stdlib



modeNone        = 0x00 :: Uint8
mode_N1_G       = 0x11 :: Uint8
mode_N2         = 0x20 :: Uint8
mode_N2_G       = 0x21 :: Uint8

errorNone       = 0x00 :: Uint8
errorGenerator  = 0x01 :: Uint8

maxAttempt      = 3    :: Uint8

srcNone         = 0xff :: Uint8


data ATS = ATS
    { mode            :: Value    Uint8
    , error           :: Values 4 Uint8
    , source          :: Value    Uint8
    , attempt         :: Value    Uint8
    , timestamp       :: Value    Uint32
    , reset           :: Value    IBool
    , clock           :: SystemClock
    , payload         :: Buffer 8 Uint8
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
    error           <- values "ats_error"            [errorNone, errorNone, errorNone, errorNone]
    source          <- value  "ats_source"           srcNone
    attempt         <- value  "ats_attempt"          0
    timestamp       <- value  "ats_timestamp"        0
    synced          <- value  "ats_synced"           false
    reset           <- value  "ats_reset"            false
    payload         <- buffer "ats_payload"
    pure ATS { mode
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
    store (payload ! 0) 0x4
    store (payload ! 1) =<< deref mode
    store (payload ! 2) =<< deref source
    store (payload ! 3) =<< deref attempt
    store (payload ! 4) =<< deref (error ! 0)
    store (payload ! 5) =<< deref (error ! 1)
    store (payload ! 6) =<< deref (error ! 2)
    store (payload ! 7) =<< deref (error ! 3)
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
    noError   <- iNot <$> hasError a
    ifte_ noError
          (do
                hasVoltage' <- deref $ hasVoltage ~> DI.state
                source'     <- deref source
                ifte_ hasVoltage'
                    (do
                        cond_ [ n <? source' ==> do
                                    delayTurnOn relay 1_000 timestamp
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
    noError   <- iNot <$> hasError a
    ifte_ noError
          (do
                hasVoltage' <- deref $ hasVoltage ~> DI.state
                isStarted'  <- deref $ start ~> R.state
                attempt'    <- deref attempt
                source'     <- deref source
                t <- deref $ start ~> R.timestamp
                cond_ [ n <? source' ==> do
                            justTurnOff relay timestamp
                            when (iNot isStarted' .&& timestamp - t >? 60_000) $ store attempt 0
                            store source n
                            store synced false

                       , n ==? source' ==> do
                            ifte_ isStarted'
                                (do
                                    isOn <- deref $ relay ~> R.state
                                    when (isOn .&& attempt' /=? 0 .&& timestamp - t >? 60_000) $ do
                                        store attempt 0
                                        store synced false
                                    ifte_ hasVoltage'
                                        (delayTurnOn relay 5_000 timestamp)
                                        (do
                                            when isOn $ store (start ~> R.timestamp) timestamp
                                            when (timestamp - t >? 30_000) $ justTurnOff start timestamp
                                            justTurnOff relay timestamp
                                        )
                                )
                                (do
                                    justTurnOff relay timestamp
                                    error' <- deref $ error ! 0
                                    when (error' ==? errorNone) $ do
                                        t <- deref $ start ~> R.timestamp
                                        when (timestamp - t >? 10_000) $
                                            cond_ [ attempt' ==? 0 ==> do
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
                            justTurnOff relay timestamp
                            delayTurnOff start 30_000 timestamp
                       ]

          )
          (do
                justTurnOff relay timestamp
                delayTurnOff start 30_000 timestamp
          )


hasError :: ATS -> Ivory eff IBool
hasError ATS{..} = do
    (.||) <$> ((.||) <$> e 1 <*> e 2) <*> e 3
    where e i = (/=? errorNone) <$> deref (error ! i)

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
        when (t1 - t0 >? 1_000) $ do
            isRelayOn'  <- deref $ isRelayOn ~> DI.state
            relayState' <- deref $ relay ~> R.state
            when (relayState' /=? isRelayOn') $ do
                store (error ! toIx n) $ safeCast relayState' * 2 + safeCast isRelayOn'
                store synced false



manageReset :: ATS
            -> Record DI.DInputStruct
            ->  Ivory eff ()
manageReset ATS{..} di = do
    isPressed   <- deref $ di ~> DI.state
    shouldReset <- deref reset
    when (iNot isPressed .&& shouldReset) $ do
        store (error ! 0) errorNone
        store (error ! 1) errorNone
        store (error ! 2) errorNone
        store (error ! 3) errorNone
        store source srcNone
        store attempt 0
        store synced false
    store reset isPressed



justTurnOn :: Record R.RelayStruct -> Uint32 -> Ivory eff ()
justTurnOn relay timestamp = do
    isOn <- deref $ relay ~> R.state
    when (iNot isOn) $ store (relay ~> R.timestamp) timestamp
    store (relay ~> R.state    ) true
    store (relay ~> R.delayOff ) 0
    store (relay ~> R.delayOn  ) 0



delayTurnOn :: Record R.RelayStruct -> Uint32 -> Uint32 -> Ivory eff ()
delayTurnOn relay delay timestamp = do
    isOn    <- deref $ relay ~> R.state
    delayOn <- deref $ relay ~> R.delayOn
    when (iNot isOn) $ do
        store (relay ~> R.delayOn) delay
        when (delayOn ==? 0) $ store (relay ~> R.timestamp) timestamp
    store (relay ~> R.delayOff ) 0



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
    when isOn $ do
        store (relay ~> R.delayOff ) delay
        when (delayOff ==? 0) $ store (relay ~> R.timestamp) timestamp
    store (relay ~> R.delayOn  ) 0



syncATS :: ATS -> Ivory (ProcEffects s ()) ()
syncATS ats = do
    synced' <- deref $ synced ats
    when (iNot synced') $ do
        transmit ats =<< message ats
        store (synced ats) true
