{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.DS18B20 where

import           Control.Monad.Reader
import           Control.Monad.State      (MonadState)
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Core.Task
import           Data.Value
import           Interface.GPIO.OpenDrain (OpenDrain)
import           Interface.MCU            (peripherals)
import           Interface.OneWire        (OneWire, read, reset, skipROM, write)
import           Interface.Timer
import           Ivory.Language
import           Prelude                  hiding (read)



data DS18B20 = DS18B20
    { name    :: String
    , onewire :: OneWire
    }


ds18b20 :: (MonadState Context m, MonadReader (Domain p t) m, OpenDrain od)
        => (p -> m od -> m OneWire) -> (p -> m od) -> m Feature
ds18b20 ow od = do
    let name  = "ds18b20"
    mcu'     <- asks    $ peripherals . mcu
    onewire  <- ow mcu' $ od mcu'
    let ds = DS18B20 { name, onewire }

    addTask $ delay      10000     (name <> "_measure_temperature") $ measureTemperature ds
    addTask $ delayPhase 10000 700 (name <> "_get_temperature"    ) $ getTemperature     ds

    pure $ Feature ds



measureTemperature :: DS18B20 -> Ivory eff ()
measureTemperature DS18B20{..} = do
    reset    onewire
    skipROM  onewire
    write    onewire 0x44


getTemperature :: DS18B20 -> Ivory eff ()
getTemperature DS18B20{..} = do
    reset    onewire
    skipROM  onewire
    write    onewire 0xbe
    replicateM_ 9 $ read onewire



instance Controller DS18B20
