{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}

module Device.GD32F3x0.Touch where

import           Control.Monad.State
import           Core.Context
import           Core.Handler
import           Core.Task
import           Data.Value
import           Device.GD32F3x0.EXTI
import           Device.GD32F3x0.Timer
import qualified Interface.Counter              as I
import qualified Interface.Touch                as I
import           Ivory.Language                 hiding (setBit)
import           Ivory.Stdlib                   (ifte, when)
import           Ivory.Support                  (symbol)
import           Support.Device.GD32F3x0.EXTI
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.SYSCFG


stateWaitStart   = 0
stateMeasuring   = 1
stateIsMeasured  = 2

deltaMinMax   = 30

data Touch = Touch { port             :: GPIO_PERIPH
                   , pin              :: GPIO_PIN
                   , srcPort          :: EXTI_PORT
                   , srcPin           :: EXTI_PIN
                   , ex               :: EXTI_LINE
                   , extiIRQ          :: IRQn
                   , timer            :: Timer
                   , stateMeasurement :: Value Uint8
                   , calculateBound   :: Value IBool
                   , timestamp1       :: Value Uint16
                   , timestamp2       :: Value Uint16
                   , timeMin          :: Value IFloat
                   , timeMax          :: Value IFloat
                   , time             :: Value IFloat
                   , stateBtn         :: Value IBool
                   , debugVal         :: Value IFloat
                   }


mkTouch :: (MonadState Context m)
        => GPIO_PERIPH -> GPIO_PIN -> RCU_PERIPH
        -> IRQn -> EXTI_PORT -> EXTI_PIN
        -> EXTI_LINE -> m Touch
mkTouch port pin rcuPin extiIRQ srcPort srcPin ex = do

    timer <- cfg_timer_14 84_000_000 0xffff_ffff

    let name = symbol srcPort <> "_" <> symbol srcPin

    timestamp1        <- value ("touch_timestamp1" <> name) 0
    timestamp2        <- value ("touch_timestamp2" <> name) 0
    timeMin           <- value ("touch_time_min" <> name) 0xffff
    timeMax           <- value ("touch_time_max" <> name) 0
    time              <- value ("touch_time" <> name) 0xffff
    stateMeasurement  <- value ("touch_state_measurement" <> name) stateWaitStart
    calculateBound    <- value ("touch_calculate_bound" <> name) false
    stateBtn          <- value ("touch_state_btn" <> name) false
    debugVal          <- value ("debug_val" <> name) 0

    addInit (symbol srcPort <> "_" <> symbol srcPin) $ do

        enablePeriphClock       rcuPin
        resetBit port pin
        modePort port pin gpio_mode_output

        enablePeriphClock       rcu_cfgcmp
        enableIrqNvic           extiIRQ 0 0
        configExtiLine          srcPort srcPin
        initExti                ex exti_interrupt exti_trig_rising
        clearExtiInterruptFlag  ex
        disableExtiInterrupt    ex

    let touch = Touch { port, pin, srcPort, srcPin, ex, extiIRQ, timer, timestamp1, timestamp2, timeMin, timeMax, time, stateMeasurement, calculateBound, stateBtn, debugVal }

    addBody (makeIRQHandlerName extiIRQ) $ handleEXTI ex $ extiHandler touch
    addTask $ delay 300 ("touch_run"<> name) $ startMeasurementMinMax touch

    pure touch


startMeasurementMinMax :: Touch -> Ivory eff ()
startMeasurementMinMax Touch{..} = do
    store calculateBound true


modePort :: GPIO_PERIPH -> GPIO_PIN -> GPIO_MODE  -> Ivory eff ()
modePort gpio pin mode = do
    setOutputOptions gpio gpio_otype_pp gpio_ospeed_50mhz pin
    setMode gpio mode gpio_pupd_none pin


extiHandler :: Touch -> Ivory eff ()
extiHandler Touch{..} = do
    f <- getExtiInterruptFlag ex
    when f $ do
        clearExtiInterruptFlag ex
        store timestamp2 =<< I.readCounter timer
        disableExtiInterrupt ex
        store stateMeasurement stateIsMeasured


instance I.Touch Touch where

    setModeInput Touch{..} =
        modePort port pin gpio_mode_input

    setModeOutput Touch{..} = do
        modePort port pin gpio_mode_output
        resetBit port pin

    getTime Touch{..} = deref debugVal

    run = runMeasurement

    getStateBtn Touch{..} = deref stateBtn


runMeasurement :: Touch -> Ivory eff () -> Ivory eff ()
runMeasurement Touch{..} handle = do
        state <- deref stateMeasurement

        when (state ==? stateWaitStart) $ do
            modePort port pin gpio_mode_input
            enableExtiInterrupt ex
            store stateMeasurement stateMeasuring
            t <- I.readCounter timer
            store timestamp1 t

        when (state ==? stateIsMeasured) $ do
            modePort port pin gpio_mode_output
            resetBit port pin

            t1 <- deref timestamp1
            t2 <- deref timestamp2
            let newTime = t2 - t1
            previousTime <- deref time
            store time $ average 0.01 previousTime $ safeCast newTime

            time' <- deref time
            calculateBound' <- deref calculateBound
            when (calculateBound' ==? true) $ do
                timeMin' <- deref timeMin
                timeMax' <- deref timeMax
                when (time' <? timeMin') $ store timeMin time'
                when (time' >? timeMax') $ store timeMax time'

                let dt = time' - timeMin'
                state <- deref stateBtn
                -- store debugVal dt
                when (dt >? 30) $ do
                    store stateBtn true
                    when (iNot state) $ store debugVal dt
                when (dt <? 20) $ do 
                    store stateBtn false
                    when state $ store debugVal dt



            -- when (timeMax' - timeMin' >? deltaMinMax) $ do
            -- ifte_ (time' >? average 0.25 timeMin' timeMax')
            --     (do store stateBtn true
            --         store debugVal =<< deref timeMax
            --     )
            --     (do store stateBtn false
            --         store debugVal =<< deref timeMin
            --     )


            store stateMeasurement stateWaitStart
            handle


average :: IFloat -> IFloat -> IFloat -> IFloat
average alpha a b = do
    a * (1 - alpha) + b * alpha
