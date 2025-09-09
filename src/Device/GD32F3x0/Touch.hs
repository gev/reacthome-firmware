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
                   , tresholdLower    :: Value Uint16
                   , tresholdUpper    :: Value Uint16
                   , timeMin          :: Value IFloat
                   , time             :: Value IFloat
                   , stateTouch       :: Value IBool
                   , debugVal         :: Value IFloat
                   }


mkTouch :: (MonadState Context m)
        => GPIO_PERIPH -> GPIO_PIN -> RCU_PERIPH
        -> IRQn -> EXTI_PORT -> EXTI_PIN
        -> EXTI_LINE -> Uint16 -> Uint16 -> m Touch
mkTouch port pin rcuPin extiIRQ srcPort srcPin ex tresholdLower' tresholdUpper' = do

    timer <- cfg_timer_14 84_000_000 0xffff_ffff

    let name = symbol srcPort <> "_" <> symbol srcPin

    tresholdLower     <- value ("touch_treshold_lower" <> name) tresholdLower'
    tresholdUpper     <- value ("touch_treshold_upper" <> name) tresholdUpper'
    timestamp1        <- value ("touch_timestamp1" <> name) 0
    timestamp2        <- value ("touch_timestamp2" <> name) 0
    timeMin           <- value ("touch_time_min" <> name) 0xffff
    time              <- value ("touch_time" <> name) 0xffff
    stateMeasurement  <- value ("touch_state_measurement" <> name) stateWaitStart
    stateTouch        <- value ("touch_state_touch" <> name) false
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
                      , tresholdLower   
                      , tresholdUpper   
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
            when (newTime >? 0) $ do
                previousTime <- deref time
                store time $ average 0.01 previousTime $ safeCast newTime

                time' <- deref time
                timeMin' <- deref timeMin
                when (time' <? timeMin') $ store timeMin time'

                let dt = time' - timeMin'
                state <- deref stateTouch
                tresholdUp <- deref tresholdUpper
                tresholdLow <- deref tresholdLower
                store debugVal dt
                when (dt >? safeCast tresholdUp) $ do
                    store stateTouch true
                    -- when (iNot state) $ store debugVal dt
                when (dt <? safeCast tresholdLow) $ do 
                    store stateTouch false
                    -- when state $ store debugVal dt

            store stateMeasurement stateWaitStart
            handle


average :: IFloat -> IFloat -> IFloat -> IFloat
average alpha a b = do
    a * (1 - alpha) + b * alpha
