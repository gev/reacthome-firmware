{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.Scd40 where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import qualified Core.Domain as D
import Core.Handler
import Core.Task
import Core.Transport
import Data.Buffer
import Data.Serialize
import Data.Value
import qualified Interface.I2C as I
import Interface.MCU
import Ivory.Language
import Ivory.Stdlib

data SCD40 = forall i. (I.I2C i 2) => SCD40
    { i2c :: i 2
    , address :: Uint8
    , startPeriodicMeasureCmd :: Values 2 Uint8
    , readMeasureCmd :: Values 2 Uint8
    , rxBuff :: Values 9 Uint8
    , txBuff :: Values 3 Uint8
    , isReady :: Value IBool
    , transmit :: forall s t. Buffer 3 Uint8 -> Ivory (ProcEffects s t) ()
    }

scd40 ::
    (MonadState Context m, MonadReader (D.Domain p c) m, I.I2C i 2, Transport t) =>
    (p -> m (i 2)) ->
    t ->
    m SCD40
scd40 i2c' transport = do
    mcu <- asks D.mcu
    i2c <- i2c' $ peripherals mcu
    startPeriodicMeasureCmd <- values "start_periodic_measure_cmd" [0x21, 0xb1]
    readMeasureCmd <- values "read_measure_cmd" [0xec, 0x05]
    rxBuff <- values_ "rx_buff"
    txBuff <- values_ "tx_buff"
    isReady <- value "is_ready" false

    let scd40 =
            SCD40
                { i2c
                , address = 0xc4
                , startPeriodicMeasureCmd
                , readMeasureCmd
                , rxBuff
                , txBuff
                , isReady
                , transmit = transmitBuffer transport
                }

    addInit "scd40_start_measuring" $ startMeasuring scd40
    addTask $ delayPhase 15_000 3_000 "scd40_get_measurement" $ getMeasurement scd40
    addTask $ delayPhase 15_000 3_010 "scd40_transmit_data" $ transmitData scd40

    addHandler $ I.HandleI2C i2c $ receive scd40

    pure scd40

startMeasuring :: SCD40 -> Ivory eff ()
startMeasuring SCD40{..} =
    I.transmit i2c address startPeriodicMeasureCmd

getMeasurement :: SCD40 -> Ivory eff ()
getMeasurement SCD40{..} = do
    store isReady false
    I.receive i2c address 9

transmitData :: SCD40 -> Ivory (ProcEffects s ()) ()
transmitData scd40 = do
    transmitHumidity scd40
    transmitTemperature scd40
    transmitCO2 scd40

transmitHumidity :: SCD40 -> Ivory (ProcEffects s ()) ()
transmitHumidity = transmit' actionHumidity $ convert calculateHumidity 6

transmitTemperature :: SCD40 -> Ivory (ProcEffects s ()) ()
transmitTemperature = transmit' actionTemperature $ convert calculateTemperature 3

transmitCO2 :: SCD40 -> Ivory (ProcEffects s ()) ()
transmitCO2 = transmit' actionCO2 $ \SCD40{..} -> do
    store (txBuff ! 1) =<< deref (rxBuff ! 1)
    store (txBuff ! 2) =<< deref (rxBuff ! 0)

transmit' ::
    Uint8 ->
    (SCD40 -> Ivory (ProcEffects s t) ()) ->
    SCD40 ->
    Ivory (ProcEffects s t) ()
transmit' action transform scd40@SCD40{..} = do
    isReady' <- deref isReady
    when isReady' $ do
        store (txBuff ! 0) action
        transform scd40
        transmit txBuff

receive :: SCD40 -> Uint8 -> Uint16 -> Ivory eff ()
receive SCD40{..} value index = do
    store (rxBuff ! toIx index) value
    when (index ==? 8) $ store isReady true

convert :: (IFloat -> IFloat) -> Ix 9 -> SCD40 -> Ivory eff ()
convert calculate index SCD40{..} =
    packLE txBuff 1 . coerce calculate =<< unpackBE rxBuff index

coerce :: (IFloat -> IFloat) -> Uint16 -> Uint16
coerce calculate = castDefault . calculate . safeCast

calculateHumidity :: IFloat -> IFloat
calculateHumidity x = 10_000 * x / 65_536

calculateTemperature :: IFloat -> IFloat
calculateTemperature x = (175 * x / 65_536 - 45) * 100
