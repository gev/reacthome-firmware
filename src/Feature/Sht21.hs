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
import qualified Core.Domain          as D
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



data SHT21 = forall i. I.I2C i 1 => SHT21
    { i2c                   :: i 1
    , address               :: Uint8
    , resetCmd              :: Values 1 Uint8
    , measureTemperatureCmd :: Values 1 Uint8
    , measureHumidityCmd    :: Values 1 Uint8
    , rxBuff                :: Values 2 Uint8
    , txBuff                :: Values 3 Uint8
    , isReady               :: Value    IBool
    , transmit              :: forall s t. Buffer 3 Uint8 -> Ivory (ProcEffects s t) ()
    }

sht21 :: (MonadState Context m, MonadReader (D.Domain p c) m, I.I2C i 1, Transport t)
        => (p -> m (i 1)) -> t -> m SHT21
sht21 i2c' transport = do
    mcu                   <- asks D.mcu
    i2c                   <- i2c' $ peripherals mcu
    resetCmd              <- values  "reset_cmd"               [0xfe]
    measureTemperatureCmd <- values  "measure_temperature_cmd" [0xf3]
    measureHumidityCmd    <- values  "measure_humidity_cmd"    [0xf5]
    rxBuff                <- values_ "rx_buff"
    txBuff                <- values_ "tx_buff"
    isReady               <- value   "is_ready"                false


    let sht21 = SHT21 { i2c, address = 0x80
                      , resetCmd, measureTemperatureCmd, measureHumidityCmd
                      , rxBuff, txBuff
                      , isReady
                      , transmit = transmitBuffer transport
                      }

    addTask $ delay      15_000       "sht21_reset"                $ reset               sht21
    addTask $ delayPhase 15_000    15 "sht21_measure_humidity"     $ measureHumidity     sht21
    addTask $ delayPhase 15_000    45 "sht21_get_humidity"         $ getData             sht21
    addTask $ delayPhase 15_000    46 "sht21_transmit_humidity"    $ transmitHumidity    sht21
    addTask $ delayPhase 15_000 2_045 "sht21_measure_temperature"  $ measureTemperature  sht21
    addTask $ delayPhase 15_000 2_130 "sht21_get_temperature"      $ getData             sht21
    addTask $ delayPhase 15_000 2_131 "sht21_transmit_temperature" $ transmitTemperature sht21

    addHandler $ I.HandleI2C i2c $ receive sht21

    pure sht21



reset :: SHT21 -> Ivory eff ()
reset SHT21{..} = I.transmit i2c address resetCmd



measureTemperature :: SHT21 -> Ivory eff ()
measureTemperature SHT21{..} = I.transmit i2c address measureTemperatureCmd


measureHumidity :: SHT21 -> Ivory eff ()
measureHumidity SHT21{..} = I.transmit i2c address measureHumidityCmd



getData :: SHT21 -> Ivory eff ()
getData SHT21{..} = do
    store isReady false
    I.receive i2c address 2



transmitHumidity :: SHT21 -> Ivory (ProcEffects s ()) ()
transmitHumidity = transmit' actionHumidity calculateHumidity


transmitTemperature :: SHT21 -> Ivory (ProcEffects s ()) ()
transmitTemperature = transmit' actionTemperature calculateTemperature


transmit' :: Uint8
          -> (IFloat -> IFloat)
          -> SHT21
          -> Ivory (ProcEffects s t) ()
transmit' action calculate sht21@SHT21{..} = do
    isReady' <- deref isReady
    when isReady' $ do
        store (txBuff ! 0) action
        packLE txBuff 1 . coerce calculate =<< unpackBE rxBuff 0
        transmit txBuff



receive :: SHT21 -> Uint8 -> Uint16 -> Ivory eff ()
receive SHT21{..} value index = do
    store (rxBuff ! toIx index) value
    when (index ==? 1) $ store isReady true



convert :: IFloat -> IFloat -> IFloat -> IFloat
convert a b x = (a + b * x / 65_536) * 100


coerce :: (IFloat -> IFloat) -> Uint16 -> Uint16
coerce calculate = castDefault . calculate . safeCast


calculateTemperature :: IFloat -> IFloat
calculateTemperature = convert (-46.88) 175.72


calculateHumidity :: IFloat -> IFloat
calculateHumidity = convert (-6.0) 125.0
