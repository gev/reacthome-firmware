{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Implementation.LeakSensor where

import           Control.Monad.Reader
import           Control.Monad.State
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Handler          (Handler (addHandler))
import           Core.Task
import           Data.Color
import           Data.Display.Canvas1D
import           Data.Record
import           Data.Value
import           Feature.DumbLEDs
import           Interface.ADC
import           Interface.Display
import           Interface.GPIO.Output
import           Interface.GPIO.Port
import           Interface.MCU
import           Ivory.Language
import           Ivory.Stdlib

voltage = 3.3
thresholdHighVoltage = 2
thresholLowVoltage = thresholdHighVoltage - 0.1


data Leak = forall o a. (Output o, ADC a) => Leak
            { sensor      :: a
            , powerSensor :: o
            , out         :: o
            , leds        :: DumbLEDs 4
            }

leakSensor :: (MonadState Context m,
               MonadReader (Domain p c) m,
               Output o, ADC a, Pull p u, Display d, Handler (Render 12) d)
            => (p -> m a)
            -> (p -> u -> m o)
            -> (p -> u -> m o)
            -> (p -> m d)
            -> m Leak
leakSensor sensor' powerSensor' out' display' = do
    let name          = "leak"
    mcu               <- asks mcu
    let peripherals'  = peripherals mcu
    sensor            <- sensor' peripherals'
    powerSensor       <- powerSensor' peripherals' $ pullNone peripherals'
    out               <- out' peripherals' $ pullNone peripherals'
    leds              <- mkDumbLEDs display'

    let leak = Leak {sensor, powerSensor, out, leds}

    addInit name $ hasntLeak leak


    addTask $ delayPhase 250 1 (name <> "_turn_on_sensor") $ taskTurnOnSensor leak 
    addTask $ delayPhase 250 2 name $ taskLeak leak
    addTask $ delayPhase 250 3 (name <> "_turn_off_sensor") $ taskTurnOffSensor leak 

    pure leak


taskTurnOnSensor :: Leak -> Ivory eff ()
taskTurnOnSensor Leak{..} = set powerSensor


taskTurnOffSensor :: Leak -> Ivory eff ()
taskTurnOffSensor Leak{..} = reset powerSensor


taskLeak :: Leak  -> Ivory (ProcEffects s ()) ()
taskLeak l@Leak{..} = do
    a <- getReduced sensor
    let v = a * voltage
    when (v <? thresholLowVoltage) (hasLeak l)
    when (v >? thresholdHighVoltage) (hasntLeak l)




hasLeak :: Leak -> Ivory (ProcEffects s ()) ()
hasLeak Leak{..} = do
    set out
    setAllColorsHSV leds 0 1 1


hasntLeak :: Leak -> Ivory (ProcEffects s ()) ()
hasntLeak Leak{..} = do
    reset out
    setAllColorsHSV leds 120 1 0.05
