{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
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
import           Support.CMSIS.CoreCMFunc       (disableIRQ, enableIRQ)
import           Support.Device.GD32F3x0.EXTI
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.SYSCFG



data Touch = Touch { port          :: GPIO_PERIPH
                   , pin           :: GPIO_PIN
                   , srcPort       :: EXTI_PORT
                   , srcPin        :: EXTI_PIN
                   , ex            :: EXTI_LINE
                   , extiIRQ       :: IRQn
                   , timer         :: Timer
                   , thresholdLow  :: IFloat
                   , thresholdHigh :: IFloat
                   , timeMin       :: Value IFloat
                   , time          :: Value IFloat
                   , stateTouch    :: Value IBool
                   , debugVal      :: Value IFloat
                   }


mkTouch :: (MonadState Context m)
        => GPIO_PERIPH -> GPIO_PIN -> RCU_PERIPH
        -> IRQn -> EXTI_PORT -> EXTI_PIN
        -> EXTI_LINE -> IFloat -> IFloat -> m Touch
mkTouch port pin rcuPin extiIRQ srcPort srcPin ex thresholdLow thresholdHigh = do

    timer <- cfg_timer_14 84_000_000 0xffff_ffff

    let name = symbol srcPort <> "_" <> symbol srcPin

    timeMin           <- value ("touch_time_min" <> name) 0xffff
    time              <- value ("touch_time" <> name) 0xffff
    stateTouch        <- value ("touch_state_touch" <> name) false
    debugVal          <- value ("debug_val" <> name) 0

    addInit (symbol srcPort <> "_" <> symbol srcPin) $ do

        enablePeriphClock       rcuPin
        modePort port pin gpio_mode_output
        resetBit port pin

    let touch = Touch { port
                      , pin
                      , srcPort
                      , srcPin
                      , ex
                      , extiIRQ
                      , timer
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

instance I.Touch Touch where
    run = runMeasurement
    getDebug Touch{..} = deref debugVal
    getState Touch{..} = deref stateTouch

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


average :: IFloat -> IFloat -> IFloat -> IFloat
average alpha a b = do
    a * (1 - alpha) + b * alpha
