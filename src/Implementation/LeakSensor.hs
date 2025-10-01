module Implementation.LeakSensor where

import Control.Monad.Reader
import Control.Monad.State
import Core.Context
import Core.Domain
import Core.Handler (Handler)
import Core.Task
import Feature.LeakLEDs
import Interface.ADC
import Interface.Display
import Interface.GPIO.Output
import Interface.GPIO.Port
import Interface.MCU
import Ivory.Language
import Ivory.Stdlib

voltage = 3.3
thresholdHighVoltage = 2
thresholdLowVoltage = thresholdHighVoltage - 0.1

data Leak = forall o a. (Output o, ADC a) => Leak
    { sensor :: a
    , powerSensor :: o
    , out :: o
    , leds :: LeakLEDs 4
    }

leakSensor ::
    ( MonadState Context m
    , MonadReader (Domain p c) m
    , Output o
    , ADC a
    , Pull p u
    , Display d
    , Handler (Render 12) d
    ) =>
    (p -> m a) ->
    (p -> u -> m o) ->
    (p -> u -> m o) ->
    (p -> m d) ->
    m Leak
leakSensor sensor' powerSensor' out' display' = do
    let name = "leak"
    mcu <- asks mcu
    let peripherals' = peripherals mcu
    sensor <- sensor' peripherals'
    powerSensor <- powerSensor' peripherals' $ pullNone peripherals'
    out <- out' peripherals' $ pullNone peripherals'
    leds <- mkLeakLEDs display'

    let leak = Leak{sensor, powerSensor, out, leds}

    addInit name $ hasntLeak leak

    addTask $ delayPhase 250 1 (name <> "_turn_on_sensor") $ taskTurnOnSensor leak
    addTask $ delayPhase 250 2 name $ taskLeak leak
    addTask $ delayPhase 250 3 (name <> "_turn_off_sensor") $ taskTurnOffSensor leak

    pure leak

taskTurnOnSensor :: Leak -> Ivory eff ()
taskTurnOnSensor Leak{..} = set powerSensor

taskTurnOffSensor :: Leak -> Ivory eff ()
taskTurnOffSensor Leak{..} = reset powerSensor

taskLeak :: Leak -> Ivory (ProcEffects s ()) ()
taskLeak l@Leak{..} = do
    a <- getReduced sensor
    let v = a * voltage
    cond_
        [ v <? thresholdLowVoltage ==> hasLeak l
        , v >? thresholdHighVoltage ==> hasntLeak l
        ]

hasLeak :: Leak -> Ivory (ProcEffects s ()) ()
hasLeak Leak{..} = do
    set out
    hasLeakLED leds

hasntLeak :: Leak -> Ivory (ProcEffects s ()) ()
hasntLeak Leak{..} = do
    reset out
    hasntLeakLED leds
