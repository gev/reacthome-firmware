{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Feature.Sht21 where

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
import Data.Serialize



data SHT21 = forall i. I.I2C i 1 => SHT21
    { i2c                  :: i 1
    , address              :: Uint8
    , resetCmd             :: Values 1 Uint8
    , mesureTemperatureCmd :: Values 1 Uint8
    , mesureHumidityCmd    :: Values 1 Uint8
    , rxBuff               :: Values 2 Uint8
    , txBuff               :: Values 3 Uint8
    , isReady              :: Value    IBool
    , transmit             :: forall s t. Buffer 3 Uint8 -> Ivory (ProcEffects s t) ()
    }

sht21 :: (MonadState Context m, MonadReader (D.Domain p t) m, I.I2C i 1, Transport t)
      => (p -> m (i 1)) -> Uint8 -> m Feature
sht21 i2c' address = do
    mcu                  <- asks D.mcu
    i2c                  <- i2c' $ peripherals mcu
    transport            <- asks D.transport
    resetCmd             <- values  "reset_cmd"              [0xfe]
    mesureTemperatureCmd <- values  "mesure_temperature_cmd" [0xf3]
    mesureHumidityCmd    <- values  "mesure_humidity_cmd"    [0xf5]
    rxBuff               <- values_ "rx_buff"
    txBuff               <- values_ "tx_buff"
    isReady              <- value   "is_ready"               false


    let sht21 = SHT21 { i2c, address
                      , resetCmd, mesureTemperatureCmd, mesureHumidityCmd
                      , rxBuff, txBuff
                      , isReady
                      , transmit = transmitBuffer transport
                      }

    addTask $ delay      5_000       "sht21_reset"                $ reset               sht21
    addTask $ delayPhase 5_000 15    "sht21_mesure_humidity"      $ mesureHumidity      sht21
    addTask $ delayPhase 5_000 45    "sht21_get_humidity"         $ getData             sht21
    addTask $ delayPhase 5_000 46    "sht21_transmit_humidity"    $ transmitHumidity    sht21
    addTask $ delayPhase 5_000 2_045 "sht21_mesure_temperature"   $ mesureTemperature   sht21
    addTask $ delayPhase 5_000 2_130 "sht21_get_temperature"      $ getData             sht21
    addTask $ delayPhase 5_000 2_131 "sht21_transmit_temperature" $ transmitTemperature sht21

    addHandler $ I.HandleI2C i2c $ receive sht21

    pure $ Feature sht21



reset :: SHT21 -> Ivory eff ()
reset SHT21{..} = I.transmit i2c address resetCmd

mesureTemperature :: SHT21 -> Ivory eff ()
mesureTemperature SHT21{..} = I.transmit i2c address mesureTemperatureCmd

mesureHumidity :: SHT21 -> Ivory eff ()
mesureHumidity SHT21{..} = I.transmit i2c address mesureHumidityCmd

getData :: SHT21 -> Ivory eff ()
getData SHT21{..} = do
    store isReady false
    I.receive i2c address 2




transmitHumidity :: SHT21 -> Ivory (ProcEffects s ()) ()
transmitHumidity sht21@SHT21{..} = 
    transmit' sht21 actionHumidity calculateHumidity


transmitTemperature :: SHT21 -> Ivory (ProcEffects s ()) ()
transmitTemperature sht21@SHT21{..} = do
    transmit' sht21 actionTemperature calculateTemperature


transmit' :: SHT21 
          -> Uint8 
          -> (SHT21 -> Ivory (ProcEffects s t) Uint16)
          -> Ivory (ProcEffects s t) ()
transmit' sht21@SHT21{..} action calculate = do
    isReady' <- deref isReady
    when isReady' $ do
        store (txBuff ! 0) action
        value <- calculate sht21
        packLE txBuff 1 value
        transmit txBuff



receive :: SHT21 -> Uint8 -> Uint16 -> Ivory eff ()
receive SHT21{..} value index = do
    store (rxBuff ! toIx index) value
    when (index ==? 1) $ store isReady true




magic :: IFloat -> IFloat -> SHT21 -> Ivory eff Uint16
magic a b SHT21{..}= do
    v0 <- safeCast <$> deref (rxBuff ! 0)
    v1 <- safeCast <$> deref (rxBuff ! 1)
    let x = (v0 `iShiftL` 8) .| v1 :: Uint16
    pure . castDefault $ (a + b * safeCast x / 65_536) * 100


calculateTemperature :: SHT21 -> Ivory eff Uint16
calculateTemperature = magic (-46.88) 175.72


calculateHumidity :: SHT21 -> Ivory eff Uint16
calculateHumidity = magic (-6.0) 125.0

--     sht21_temperature_action.temperature = sht21(-46.85, 175.72, res) + correction;
--     sht21_humidity_action.humidity = sht21(-6.0, 125.0, res);


-- uint16_t sht21(double a, double b, uint8_t * raw)
-- {
--     uint16_t x = (((uint16_t) raw[0]) << 8) | (raw[1] & 0xfc);
--     return (a + b * x / 65536) * 100;
-- }

instance Controller SHT21


