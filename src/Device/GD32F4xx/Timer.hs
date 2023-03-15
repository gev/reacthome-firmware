{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Device.GD32F4xx.Timer where

import           Core.Context
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


timer_1 :: TIMER_PARAM -> Timer
timer_1 = Timer TIMER1 RCU_TIMER1 TIMER1_IRQn

timer_2 :: TIMER_PARAM -> Timer
timer_2 = Timer TIMER2 RCU_TIMER2 TIMER2_IRQn


instance Include Timer where
    include (Timer {..}) = include initTimer'
        where
            initTimer' :: Def ('[] ':-> ())
            initTimer' = proc (show timer <> "_init") $ body $ do
                enablePeriphClock rcu
                deinitTimer       timer
                initTimer         timer param
                enableTimer       timer


instance I.Counter Timer where
    readCounter = readCounter . timer



instance Include (I.HandleTimer Timer) where
    include (I.HandleTimer {I.timer = Timer {..}, handle}) = do
        include $ makeIRQHandler timer (handleIRQ timer handle)
        include initTimerIRQ'
        where
            initTimerIRQ' :: Def ('[] ':-> ())
            initTimerIRQ' = proc (show timer <> "_irq_init") $ body $ do
                enableIrqNvic irq 0 0
                enableTimerInterrupt timer TIMER_INT_UP


handleIRQ :: TIMER_PERIPH -> Ivory eff () -> Ivory eff ()
handleIRQ timer handle = do
    flag <- getTimerInterruptFlag timer TIMER_INT_FLAG_UP
    when flag $ clearTimerInterruptFlag timer TIMER_INT_FLAG_UP
    handle
