{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Feature.Scd40 where

import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State  (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Controller      (Controller)
import qualified Core.Domain          as D
import           Core.Feature
import           Core.Handler
import           Core.Task
import           Core.Transport
import           Data.Buffer
import           Data.Value
import qualified Interface.I2C        as I
import           Interface.MCU
import           Ivory.Language
import           Ivory.Stdlib
import           Data.Serialize



data SCD40 = forall i. I.I2C i 2 => SCD40
    { i2c                        :: i 2
    , address                   :: Uint8
    , startPeriodicMeasureCmd   :: Values 2 Uint8
    , readMeasureCmd            :: Values 2 Uint8
    , rxBuff                    :: Values 9 Uint8
    , txBuff                    :: Values 3 Uint8
    , isMeasured                    :: Value    IBool
    , isReady                   :: Value    IBool
    , transmit                  :: forall s t. Buffer 3 Uint8 -> Ivory (ProcEffects s t) ()
    }

scd40 :: (MonadState Context m, MonadReader (D.Domain p t) m, I.I2C i 2, Transport t)
      => (p -> m (i 2)) -> Uint8 -> m Feature
scd40 i2c' address = do
    mcu                         <- asks D.mcu
    i2c                         <- i2c' $ peripherals mcu
    transport                   <- asks D.transport
    startPeriodicMeasureCmd     <- values  "start_periodic_measure_cmd" [0x21, 0xb1]
    readMeasureCmd              <- values  "read_mesure_cmd"            [0xec, 0x05]
    rxBuff                      <- values_ "rx_buff"
    txBuff                      <- values_ "tx_buff"
    isMeasured                  <- value   "is_init"                    false
    isReady                     <- value   "is_ready"                   false


    let scd40 = SCD40 { i2c, address
                      , startPeriodicMeasureCmd, readMeasureCmd 
                      , rxBuff, txBuff
                      , isMeasured, isReady
                      , transmit = transmitBuffer transport
                      }

    addTask $ delay      5_000       "scd40_start_measuring"      $ startMeasuring      scd40
    addTask $ delayPhase 15_000 15   "scd40_get_measurement"      $ getMeasurement      scd40
    addTask $ delayPhase 15_000 20   "scd40_transmit_humidity"    $ transmitHumidity    scd40
    addTask $ delayPhase 15_000 25   "scd40_transmit_temperature" $ transmitTemperature scd40
    addTask $ delayPhase 15_000 30   "scd40_transmit_co2"         $ transmitCO2         scd40


    addHandler $ I.HandleI2C i2c $ receive scd40

    pure $ Feature scd40



startMeasuring :: SCD40 -> Ivory eff ()
startMeasuring SCD40{..} = do 
    isMeasured' <- deref isMeasured
    when (isMeasured' ==? false) $ do
        I.transmit i2c address startPeriodicMeasureCmd
        store isMeasured true



getMeasurement :: SCD40 -> Ivory eff ()
getMeasurement SCD40{..} = do
    store isReady false
    I.receive i2c address 9



transmitHumidity :: SCD40 -> Ivory (ProcEffects s ()) ()
transmitHumidity scd40@SCD40{..} = do
    v0 <- safeCast <$> deref (rxBuff ! 6)
    v1 <- safeCast <$> deref (rxBuff ! 7)
    let x = (v0 `iShiftL` 8) .| v1 :: Uint16
    let value = castDefault $ ((10_000 :: IFloat) * safeCast x) / 65_536 :: Uint16
    transmit' scd40 actionHumidity value


transmitTemperature :: SCD40 -> Ivory (ProcEffects s ()) ()
transmitTemperature scd40@SCD40{..} = do
    v0 <- safeCast <$> deref (rxBuff ! 3)
    v1 <- safeCast <$> deref (rxBuff ! 4)
    let x = (v0 `iShiftL` 8) .| v1 :: Uint16
    let value = castDefault $ (((175 :: IFloat) * safeCast x) / 65_536 - 45) * 100
    transmit' scd40 actionTemperature value


transmitCO2 :: SCD40 -> Ivory (ProcEffects s ()) ()
transmitCO2 scd40@SCD40{..} = do
    v0 <- safeCast <$> deref (rxBuff ! 0)
    v1 <- safeCast <$> deref (rxBuff ! 1)
    let value = (v0 `iShiftL` 8) .| v1 :: Uint16
    transmit' scd40 actionCo2 value


transmit' :: SCD40 
          -> Uint8 
          -> Uint16
          -> Ivory (ProcEffects s t) ()
transmit' scd40@SCD40{..} action value = do
    isReady' <- deref isReady
    when isReady' $ do
        store (txBuff ! 0) action
        packLE txBuff 1 value
        transmit txBuff



receive :: SCD40 -> Uint8 -> Uint16 -> Ivory eff ()
receive SCD40{..} value index = do
    store (rxBuff ! toIx index) value
    when (index ==? 1) $ store isReady true



instance Controller SCD40


