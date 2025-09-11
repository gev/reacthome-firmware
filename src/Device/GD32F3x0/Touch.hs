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
import           Ivory.Stdlib.Control
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

data Touch = Touch { port             :: GPIO_PERIPH
                   , pin              :: GPIO_PIN
                   , srcPort          :: EXTI_PORT
                   , srcPin           :: EXTI_PIN
                   , ex               :: EXTI_LINE
                   , extiIRQ          :: IRQn
                   , timer            :: Timer
                   , stateMeasurement :: Value Uint8
                   , timestamp1       :: Value Uint16
                   , timestamp2       :: Value Uint16
                   , thresholdLow     :: IFloat
                   , thresholdHigh    :: IFloat
                   , timeMin          :: Value IFloat
                   , time             :: Value IFloat
                   , stateTouch       :: Value IBool
                   , debugVal         :: Value IFloat
                   }


mkTouch :: (MonadState Context m)
        => GPIO_PERIPH -> GPIO_PIN -> RCU_PERIPH
        -> IRQn -> EXTI_PORT -> EXTI_PIN
        -> EXTI_LINE -> IFloat -> IFloat -> m Touch
mkTouch port pin rcuPin extiIRQ srcPort srcPin ex thresholdLow thresholdHigh = do

    timer <- cfg_timer_14 84_000_000 0xffff_ffff

    let name = symbol srcPort <> "_" <> symbol srcPin

    timestamp1        <- value ("touch_timestamp1" <> name) 0
    timestamp2        <- value ("touch_timestamp2" <> name) 0
    timeMin           <- value ("touch_time_min" <> name) 0xffff
    time              <- value ("touch_time" <> name) 0xffff
    stateMeasurement  <- value ("touch_state_measurement" <> name) stateWaitStart
    stateTouch        <- value ("touch_state_touch" <> name) false
    debugVal          <- value ("debug_val" <> name) 0

    addInit (symbol srcPort <> "_" <> symbol srcPin) $ do

        enablePeriphClock       rcuPin
        modePort port pin gpio_mode_output
        resetBit port pin

        -- enablePeriphClock       rcu_gpiob 
        -- modePort gpiob gpio_pin_6 gpio_mode_output
        -- resetBit gpiob gpio_pin_6 --internal

        -- enablePeriphClock       rcu_gpioa
        -- modePort gpioa gpio_pin_8 gpio_mode_output
        -- setBit gpioa gpio_pin_8 --external

        enablePeriphClock       rcu_cfgcmp
        enableIrqNvic           extiIRQ 0 0
        configExtiLine          srcPort srcPin
        initExti                ex exti_interrupt exti_trig_rising
        clearExtiInterruptFlag  ex
        disableExtiInterrupt    ex

    let touch = Touch { port
                      , pin
                      , srcPort
                      , srcPin
                      , ex
                      , extiIRQ
                      , timer
                      , stateMeasurement
                      , timestamp1
                      , timestamp2
                      , thresholdLow
                      , thresholdHigh
                      , timeMin
                      , time
                      , stateTouch
                      , debugVal
                      }

    addBody (makeIRQHandlerName extiIRQ) $ handleEXTI ex $ extiHandler touch

    pure touch


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

    getTime Touch{..} = deref debugVal

    run = runMeasurement

    getState Touch{..} = deref stateTouch


runMeasurement :: Touch -> Ivory (ProcEffects s ()) () -> Ivory (ProcEffects s ()) ()
runMeasurement Touch{..} handle = do
        -- toggleBit gpiob gpio_pin_6

        state <- deref stateMeasurement
        when (state ==? stateWaitStart) $ do
            -- modePort gpiob gpio_pin_6 gpio_mode_input

            modePort port pin gpio_mode_input
            enableExtiInterrupt ex
            store stateMeasurement stateMeasuring
            t <- I.readCounter timer
            store timestamp1 t

        when (state ==? stateIsMeasured) $ do
            -- modePort gpiob gpio_pin_6 gpio_mode_output
            -- resetBit gpiob gpio_pin_6

            modePort port pin gpio_mode_output
            resetBit port pin

            -- t1' <- I.readCounter timer
            -- forever $ do
            --     t2' <- I.readCounter timer
            --     when (t2' - t1' >? 400) breakOut


            t1 <- deref timestamp1
            t2 <- deref timestamp2
            let newTime = safeCast $ t2 - t1
            when (newTime >? 0) $ do

                time' <- deref time
                timeMin' <- deref timeMin
                when (time' <? timeMin') $ store timeMin time'

                let dt = time' - timeMin'

                previousTime <- deref time
                store time (average 0.01 previousTime newTime)
                -- cond_ [ dt >? thresholdHigh ==> store time (average 0.05 previousTime newTime)
                --       , dt >? thresholdLow ==> store time (average 0.01 previousTime newTime)
                --       , true ==> store time (average 0.005 previousTime newTime)
                --       ]

                store debugVal dt
                -- store debugVal $ 1000 + dt

                -- state <- deref stateTouch
                when (dt >? thresholdHigh) $ do
                    store stateTouch true
                    -- when (iNot state) $ store debugVal dt
                when (dt <? thresholdLow) $ do
                    store stateTouch false
                    -- when state $ store debugVal dt

            store stateMeasurement stateWaitStart
            handle


average :: IFloat -> IFloat -> IFloat -> IFloat
average alpha a b = do
    a * (1 - alpha) + b * alpha
