{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE RecordWildCards       #-}

module Device.GD32F3x0.Touch where

import           Control.Monad.State
import           Core.Context
import           Core.Handler
import           Core.Task
import           Data.Value
import           Device.GD32F3x0.EXTI
import           Device.GD32F3x0.Timer
import           Interface.Counter
import qualified Interface.EXTI                 as I
import           Interface.GPIO.Input
import           Ivory.Language hiding (setBit)
import           Ivory.Support         (symbol)
import           Support.Device.GD32F3x0.EXTI
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.SYSCFG

data Touch = Touch { port :: GPIO_PERIPH
                   , pin  :: GPIO_PIN
                   , timer     :: Timer
                   , srcPort   :: EXTI_PORT
                   , srcPin    :: EXTI_PIN
                   , ex        :: EXTI_LINE
                   , threshold :: Uint16
                   , timeStamp :: Value Uint16
                   , stateBtn  :: Value IBool
                   }

mkTouch :: MonadState Context m
        => (Uint32 -> Uint32 -> m Timer)
        -> GPIO_PERIPH -> GPIO_PIN -> RCU_PERIPH
        -> IRQn -> EXTI_PORT -> EXTI_PIN
        -> EXTI_LINE -> Uint16 -> m Touch
mkTouch timer' port pin rcuPin extiIRQ srcPort srcPin ex threshold = do

    timeStamp <- value ("touch_timestamp" <> symbol srcPort <> "_" <> symbol srcPin) 0
    stateBtn  <- value ("touch_state_btn" <> symbol srcPort <> "_" <> symbol srcPin) false
    stateBtn  <- value ("touch_state_btn" <> symbol srcPort <> "_" <> symbol srcPin) false
    timer     <- timer' 1_000_000 0xFF_FF

    addInit (symbol srcPort <> "_" <> symbol srcPin) $ do

        enablePeriphClock       rcuPin
        resetBit port pin
        modePort port pin gpio_mode_output

        enablePeriphClock       rcu_cfgcmp
        enableIrqNvic           extiIRQ 0 0
        configExtiLine          srcPort srcPin
        initExti                ex exti_interrupt exti_trig_rising
        clearExtiInterruptFlag  ex

    let touch = Touch { port, pin, timer, srcPort, srcPin, ex, threshold, timeStamp, stateBtn }

    addTask $ delay 1 ("touch_task" <> symbol srcPort <> "_" <> symbol srcPin) $ touchTask touch

    addBody (makeIRQHandlerName extiIRQ) $ handleEXTI ex $ extiHandler touch

    pure touch


modePort :: GPIO_PERIPH -> GPIO_PIN -> GPIO_MODE  -> Ivory eff ()
modePort gpio pin mode = do
    setOutputOptions gpio gpio_otype_pp gpio_ospeed_50mhz pin
    setMode gpio mode gpio_pupd_none pin


touchTask :: Touch -> Ivory eff ()
touchTask Touch{..} = do
    count <- readCounter timer
    modePort port pin gpio_mode_input
    store timeStamp count


extiHandler :: Touch -> Ivory eff ()
extiHandler Touch{..} = do
    count <- readCounter timer
    stamp <- deref timeStamp


    ifte_ (count - stamp >? threshold)
          (store stateBtn true)
          (store stateBtn false)

    modePort port pin gpio_mode_output
    resetBit port pin



instance Input Touch where
    get Touch{..} = deref stateBtn
