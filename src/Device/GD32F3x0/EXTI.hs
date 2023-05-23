{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Device.GD32F3x0.EXTI where


import           Control.Monad.Writer (MonadWriter)
import           Core.Context
import           Core.Handler
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Support
import qualified Interface.EXTI     as E
import           Device.GD32F3x0.GPIO.Input
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.EXTI
import           Support.Device.GD32F3x0.SYSCFG
import           Support.Device.GD32F3x0.Misc



data EXTI = EXTI 
    { port    :: Input
    , extiIRQ :: IRQn
    , srcPort :: EXTI_PORT
    , srcPin  :: EXTI_PIN
    , ex      :: EXTI_LINE
    }

mkEXTI :: MonadWriter Context m => m Input -> IRQn -> EXTI_PORT -> EXTI_PIN -> EXTI_LINE -> m EXTI
mkEXTI input extiIRQ srcPort srcPin ex = do
    port <- input
    let initEXTI' :: Def ('[] :-> ())
        initEXTI' = proc (symbol srcPort <> "_" <> symbol srcPin <> "_init") $ body $ do
            enablePeriphClock       rcu_cfgcmp
            enableIrqNvic           extiIRQ 0 0
            configExtiLine          srcPort srcPin
            initExti                ex exti_interrupt exti_trig_rising
            clearExtiInterruptFlag  ex
    addInit initEXTI'
    pure EXTI { port, extiIRQ, srcPort, srcPin, ex }


instance Handler E.HandleEXTI EXTI where
    addHandler (E.HandleEXTI EXTI{..} handle) = do
        addModule $ makeIRQHandler extiIRQ (handleEXTI ex handle)

handleEXTI :: EXTI_LINE -> Ivory eff () -> Ivory eff ()
handleEXTI ex handle = do
    f <- getExtiInterruptFlag ex 
    when f $ do
        clearExtiInterruptFlag ex 
        handle