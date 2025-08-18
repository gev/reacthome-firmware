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
import qualified Interface.Timer as I
import qualified Interface.Touch                as I
import           Ivory.Language                 hiding (setBit)
import           Ivory.Stdlib                   (when)
import           Ivory.Support                  (symbol)
import           Support.Device.GD32F3x0.EXTI
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.SYSCFG


data Touch = Touch { port    :: GPIO_PERIPH
                   , pin       :: GPIO_PIN
                   , srcPort   :: EXTI_PORT
                   , srcPin    :: EXTI_PIN
                   , ex        :: EXTI_LINE
                   , extiIRQ   :: IRQn
                   , timer     :: Timer
                   , timestamp :: Value Uint32
                   }

-- type TouchStruct = "touch_struct"

-- [ivory|
--     struct touch_struct
--     { port      :: GPIO_PERIPH
--     ; pin       :: GPIO_PIN
--     ; srcPort   :: EXTI_PORT
--     ; srcPin    :: EXTI_PIN
--     ; ex        :: EXTI_LINE
--     ; extiIRQ   :: IRQn
--     }
-- |]

mkTouch :: (MonadState Context m)
        => GPIO_PERIPH -> GPIO_PIN -> RCU_PERIPH
        -> IRQn -> EXTI_PORT -> EXTI_PIN
        -> EXTI_LINE -> Timer -> m Touch 
mkTouch port pin rcuPin extiIRQ srcPort srcPin ex timer = do

    timestamp <- value ("touch_timestamp" <> symbol srcPort <> "_" <> symbol srcPin) 0

    stateBtn  <- value ("touch_state_btn" <> symbol srcPort <> "_" <> symbol srcPin) false
    stateBtn  <- value ("touch_state_btn" <> symbol srcPort <> "_" <> symbol srcPin) false

    addInit (symbol srcPort <> "_" <> symbol srcPin) $ do

        enablePeriphClock       rcuPin
        resetBit port pin
        modePort port pin gpio_mode_output

        enablePeriphClock       rcu_cfgcmp
        enableIrqNvic           extiIRQ 0 0
        configExtiLine          srcPort srcPin
        initExti                ex exti_interrupt exti_trig_rising
        clearExtiInterruptFlag  ex

    let touch = Touch { port, pin, srcPort, srcPin, ex, extiIRQ, timer, timestamp }

    pure touch


modePort :: GPIO_PERIPH -> GPIO_PIN -> GPIO_MODE  -> Ivory eff ()
modePort gpio pin mode = do
    setOutputOptions gpio gpio_otype_pp gpio_ospeed_50mhz pin
    setMode gpio mode gpio_pupd_none pin


instance Handler I.HandleTouch Touch where
    addHandler (I.HandleTouch t@Touch{..} handle) = do
        addBody (makeIRQHandlerName extiIRQ) (handleTouch t handle)

handleTouch :: Touch -> Ivory eff () -> Ivory eff ()
handleTouch Touch{..} handle = do
    f <- getExtiInterruptFlag ex
    when f $ do
        clearExtiInterruptFlag ex
        disableExtiInterrupt ex
        store timestamp =<< I.getCounter timer
        handle

instance I.Touch Touch where

    setModeInput Touch{..} =
        modePort port pin gpio_mode_input

    setModeOutput Touch{..} = do
        modePort port pin gpio_mode_output
        resetBit port pin

    enable Touch{..} =
        enableExtiInterrupt ex

    disable Touch{..} =
        disableExtiInterrupt ex
