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
thresholdVoltage = 2


data Leak = forall o a. (Output o, ADC a) => Leak
            { sensor :: a
            , switch :: o
            , out    :: o
            , leds   :: DumbLEDs 4
            }

leakSensor :: (MonadState Context m,
               MonadReader (Domain p c) m,
               Output o, ADC a, Pull p u, Display d, Handler (Render 12) d)
            => (p -> m a)
            -> (p -> u -> m o)
            -> (p -> u -> m o)
            -> (p -> m d)
            -> m Leak
leakSensor sensor' switch' out' display' = do
    let name     = "leak"
    mcu          <- asks mcu
    let peripherals'  = peripherals mcu
    sensor       <- sensor' peripherals'
    switch       <- switch' peripherals' $ pullNone peripherals'
    out          <- out' peripherals' $ pullNone peripherals'
    leds         <- mkDumbLEDs display'

    let leak = Leak {sensor, switch, out, leds}

    addInit name $ hasntLeak leak


    addTask $ delay 50 (name <> "_turn_on_switch") $ taskTurnOnSwitch leak 
    addTask $ delayPhase 50 1 name $ taskLeak leak

    pure leak


taskTurnOnSwitch :: Leak -> Ivory eff ()
taskTurnOnSwitch Leak{..} = set switch


taskLeak :: Leak  -> Ivory (ProcEffects s ()) ()
taskLeak l@Leak{..} = do
    a <- getReduced sensor
    -- reset switch
    let v = a * voltage
    ifte_ (v <? thresholdVoltage)
          (hasLeak l)
          (hasntLeak l)


hasLeak :: Leak -> Ivory (ProcEffects s ()) ()
hasLeak Leak{..} = do
    set out
    setAllColorsHSV leds 0 1 0.1


hasntLeak :: Leak -> Ivory (ProcEffects s ()) ()
hasntLeak Leak{..} = do
    reset out
    setAllColorsHSV leds 120 1 0.1
