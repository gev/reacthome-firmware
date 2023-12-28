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
import           Data.Serialize
import           Data.Value
import qualified Interface.I2C        as I
import           Interface.MCU
import           Ivory.Language
import           Ivory.Stdlib



data SCD40 = forall i. I.I2C i 2 => SCD40
    { i2c                       :: i 2
    , address                   :: Uint8
    , startPeriodicMeasureCmd   :: Values 2 Uint8
    , readMeasureCmd            :: Values 2 Uint8
    , rxBuff                    :: Values 9 Uint8
    , txBuff                    :: Values 3 Uint8
    , isMeasured                :: Value    IBool
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
    readMeasureCmd              <- values  "read_measure_cmd"           [0xec, 0x05]
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

    addInit                        "scd40_start_measuring"      $ startMeasuring      scd40
    addTask $ delayPhase 15_000 15 "scd40_get_measurement"      $ getMeasurement      scd40
    addTask $ delayPhase 15_000 20 "scd40_transmit_humidity"    $ transmitHumidity    scd40
    addTask $ delayPhase 15_000 25 "scd40_transmit_temperature" $ transmitTemperature scd40
    addTask $ delayPhase 15_000 30 "scd40_transmit_co2"         $ transmitCO2         scd40

    addHandler $ I.HandleI2C i2c $ receive scd40

    pure $ Feature scd40



startMeasuring :: SCD40 -> Ivory eff ()
startMeasuring SCD40{..} = do
    isMeasured' <- deref isMeasured
    when (iNot isMeasured') $ do
        I.transmit i2c address startPeriodicMeasureCmd
        store isMeasured true



getMeasurement :: SCD40 -> Ivory eff ()
getMeasurement SCD40{..} = do
    store isReady false
    I.receive i2c address 9



transmitHumidity :: SCD40 -> Ivory (ProcEffects s ()) ()
transmitHumidity scd40@SCD40{..} =
    transmit' scd40 actionHumidity $
        packLE txBuff 1 . calculateHumidity =<< unpackBE rxBuff 6


transmitTemperature :: SCD40 -> Ivory (ProcEffects s ()) ()
transmitTemperature scd40@SCD40{..} =
    transmit' scd40 actionTemperature $
        packLE txBuff 1 . calculateTemperature =<< unpackBE rxBuff 3


transmitCO2 :: SCD40 -> Ivory (ProcEffects s ()) ()
transmitCO2 scd40@SCD40{..} =
    transmit' scd40 actionCo2 $ do
        store (txBuff ! 1) =<< deref (rxBuff ! 1)
        store (txBuff ! 2) =<< deref (rxBuff ! 0)


transmit' :: SCD40
          -> Uint8
          -> Ivory (ProcEffects s t) ()
          -> Ivory (ProcEffects s t) ()
transmit' scd40@SCD40{..} action transform = do
    isReady' <- deref isReady
    when isReady' $ do
        transform
        store (txBuff ! 0) action
        transmit txBuff



receive :: SCD40 -> Uint8 -> Uint16 -> Ivory eff ()
receive SCD40{..} value index = do
    store (rxBuff ! toIx index) value
    when (index ==? 8) $ store isReady true



magic :: (IFloat -> IFloat) -> Uint16 -> Uint16
magic calculate = castDefault . calculate . safeCast

calculateHumidity :: Uint16 -> Uint16
calculateHumidity = magic $ \x -> 10_000 *  x / 65_536


calculateTemperature :: Uint16 -> Uint16
calculateTemperature = magic $ \x -> (175 * x / 65_536 - 45) * 100



instance Controller SCD40
