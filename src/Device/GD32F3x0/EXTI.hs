{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Device.GD32F3x0.EXTI where


import           Control.Monad.State            (MonadState)
import           Core.Context
import           Core.Handler
import           Device.GD32F3x0.GPIO.Input
import qualified Interface.EXTI                 as I
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Support
import           Support.Device.GD32F3x0.EXTI
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.SYSCFG



data EXTI = EXTI
    { port    :: Input
    , extiIRQ :: IRQn
    , srcPort :: EXTI_PORT
    , srcPin  :: EXTI_PIN
    , ex      :: EXTI_LINE
    }

mkEXTI :: MonadState Context m => m Input -> IRQn -> EXTI_PORT -> EXTI_PIN -> EXTI_LINE -> m EXTI
mkEXTI input extiIRQ srcPort srcPin ex = do
    port <- input
    addInit (symbol srcPort <> "_" <> symbol srcPin) $ do
            enablePeriphClock       rcu_cfgcmp
            enableIrqNvic           extiIRQ 0 0
            configExtiLine          srcPort srcPin
            initExti                ex exti_interrupt exti_trig_rising
            clearExtiInterruptFlag  ex
    pure EXTI { port, extiIRQ, srcPort, srcPin, ex }


instance Handler I.HandleEXTI EXTI where
    addHandler (I.HandleEXTI EXTI{..} handle) = do
        addModule $ makeIRQHandler extiIRQ (handleEXTI ex handle)

handleEXTI :: EXTI_LINE -> Ivory eff () -> Ivory eff ()
handleEXTI ex handle = do
    f <- getExtiInterruptFlag ex
    when f $ do
        clearExtiInterruptFlag ex
        handle


instance I.EXTI EXTI
