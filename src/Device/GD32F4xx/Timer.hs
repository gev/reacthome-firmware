{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Device.GD32F4xx.Timer where

import           Control.Monad.Writer          (MonadWriter)
import           Core.Context
import           Core.Handler
import qualified Interface.Counter             as I
import qualified Interface.Timer               as I
import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Stdlib
import           Support.Device.GD32F4xx.IRQ
import           Support.Device.GD32F4xx.Misc
import           Support.Device.GD32F4xx.RCU
import           Support.Device.GD32F4xx.Timer



data Timer = Timer
    { timer :: TIMER_PERIPH
    , rcu   :: RCU_PERIPH
    , irq   :: IRQn
    , param :: TIMER_PARAM
    }



timer_1 :: MonadWriter Context m => TIMER_PARAM -> m Timer
timer_1 = mkTimer TIMER1 RCU_TIMER1 TIMER1_IRQn

timer_2 :: MonadWriter Context m => TIMER_PARAM -> m Timer
timer_2 = mkTimer TIMER2 RCU_TIMER2 TIMER2_IRQn



mkTimer :: MonadWriter Context m
        => TIMER_PERIPH
        -> RCU_PERIPH
        -> IRQn
        -> TIMER_PARAM
        -> m Timer
mkTimer timer rcu irq param = do
    addInit initTimer'
    pure Timer { timer, rcu, irq, param }
    where
        initTimer' :: Def ('[] ':-> ())
        initTimer' = proc (show timer <> "_init") $ body $ do
            enablePeriphClock rcu
            deinitTimer       timer
            initTimer         timer param
            enableTimer       timer



instance I.Counter Timer where
    readCounter = readCounter . timer



instance Handler I.HandleTimer Timer where
    addHandler (I.HandleTimer {I.timer = Timer {..}, handle}) = do
        addProc initTimerIRQ'
        addModule $ makeIRQHandler timer handleIRQ'
        where
            initTimerIRQ' :: Def ('[] ':-> ())
            initTimerIRQ' = proc (show timer <> "_irq_init") $ body $ do
                enableIrqNvic irq 0 0
                enableTimerInterrupt timer TIMER_INT_UP

            handleIRQ' :: Ivory eff ()
            handleIRQ' = do
                flag <- getTimerInterruptFlag timer TIMER_INT_FLAG_UP
                when flag $ clearTimerInterruptFlag timer TIMER_INT_FLAG_UP
                handle
