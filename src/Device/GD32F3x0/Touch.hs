{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RankNTypes #-}

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
import Support.CMSIS.CoreCMFunc (disableIRQ, enableIRQ)



data Touch = Touch { port             :: GPIO_PERIPH
                   , pin              :: GPIO_PIN
                   , srcPort          :: EXTI_PORT
                   , srcPin           :: EXTI_PIN
                   , ex               :: EXTI_LINE
                   , extiIRQ          :: IRQn
                   , timer            :: Timer
                   , isReady          :: Value IBool
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
    isReady           <- value ("touch_is_ready" <> name) false
    stateTouch        <- value ("touch_state_touch" <> name) false
    debugVal          <- value ("debug_val" <> name) 0

    addInit (symbol srcPort <> "_" <> symbol srcPin) $ do

        enablePeriphClock       rcuPin
        modePort port pin gpio_mode_output
        resetBit port pin

        -- enablePeriphClock       rcu_cfgcmp
        -- enableIrqNvic           extiIRQ 0 0
        -- configExtiLine          srcPort srcPin
        -- initExti                ex exti_interrupt exti_trig_rising
        -- clearExtiInterruptFlag  ex

    let touch = Touch { port
                      , pin
                      , srcPort
                      , srcPin
                      , ex
                      , extiIRQ
                      , timer
                      , isReady
                      , timestamp1
                      , timestamp2
                      , thresholdLow
                      , thresholdHigh
                      , timeMin
                      , time
                      , stateTouch
                      , debugVal
                      }

    -- addBody (makeIRQHandlerName extiIRQ) $ handleEXTI ex $ extiHandler touch

    addTask $ delay 10_000 ("reset_min" <> name) $ resetMin touch

    pure touch

resetMin :: Touch -> Ivory eff ()
resetMin Touch{..} = do
    state <- deref stateTouch
    when (iNot state) $
        store timeMin 0xffff

modePort :: GPIO_PERIPH -> GPIO_PIN -> GPIO_MODE  -> Ivory eff ()
modePort gpio pin mode = do
    setOutputOptions gpio gpio_otype_pp gpio_ospeed_50mhz pin
    setMode gpio mode gpio_pupd_none pin


-- extiHandler :: Touch -> Ivory eff ()
-- extiHandler Touch{..} = do
--     f <- getExtiInterruptFlag ex
--     when f $ do
--         state <- deref stateMeasurement
--         when (state ==? stateMeasuring) $ do
--             store timestamp2 =<< I.readCounter timer
--             store stateMeasurement stateIsMeasured
--         clearExtiInterruptFlag ex

instance I.Touch Touch where
    run = runMeasurement
    reset Touch{..} = pure()
    getDebug Touch{..} = deref debugVal
    getState Touch{..} = deref stateTouch
    start Touch{..} = do 
        store isReady false
        disableIRQ
        modePort port pin gpio_mode_input
        store timestamp1 =<< I.readCounter timer
    finish Touch{..} = do
        enableIRQ
        modePort port pin gpio_mode_output
        resetBit port pin
    isReady Touch{..} = deref isReady

runMeasurement :: Touch -> Ivory (ProcEffects s ()) ()
runMeasurement t@Touch{..} = do

    modePort port pin gpio_mode_input
    disableIRQ
    t1 <- I.readCounter timer
    forever $ do
        isMeasured <- getInputBit port pin
        when isMeasured breakOut 
    t2 <- I.readCounter timer

    enableIRQ
    modePort port pin gpio_mode_output
    resetBit port pin

    let newTime = safeCast $ t2 - t1
    previousTime <- deref time
    store time (average 0.1 previousTime newTime)
    time' <- deref time
    min'  <- deref timeMin
    when (time' <? min') $ store timeMin time'
    min'' <- deref timeMin
    let dt = time' - min''

    store debugVal dt
    when (dt >? thresholdHigh) $ do
        store stateTouch true
    when (dt <? thresholdLow) $ do
        store stateTouch false

        -- state <- deref stateMeasurement

        -- when (state ==? stateInactive) $ do
        --     store stateMeasurement stateWaitStart
        --     modePort port pin gpio_mode_output
        --     resetBit port pin
        --     handle

        -- when (state ==? stateWaitStart) $ do
        --     modePort port pin gpio_mode_input
        --     disableIRQ
        --     store timestamp1 =<< I.readCounter timer
        --     forever $ do
        --         isMeasured <- getInputBit port pin
        --         when isMeasured breakOut 
        --     store timestamp2 =<< I.readCounter timer
        --     enableIRQ
        --     store stateMeasurement stateIsMeasured

        -- isReady' <- deref isReady
        -- when (iNot isReady') $ do
        --     state <- getInputBit port pin
        --     when state $ do 
        --         store isReady true
        --     -- when (state ==? stateIsMeasured) $ do
        --         t1 <- deref timestamp1
        --         t2 <-  I.readCounter timer
        --         let newTime = safeCast $ t2 - t1
        --         previousTime <- deref time
        --         store time (average 0.01 previousTime newTime)
        --         time' <- deref time
        --         min'  <- deref timeMin
        --         when (time' <? min') $ store timeMin time'
        --         min'' <- deref timeMin
        --         let dt = time' - min''

        --         store debugVal dt
        --         when (dt >? thresholdHigh) $ do
        --             store stateTouch false
        --         when (dt <? thresholdLow) $ do
        --             store stateTouch false

            -- store stateMeasurement stateInactive


average :: IFloat -> IFloat -> IFloat -> IFloat
average alpha a b = do
    a * (1 - alpha) + b * alpha
