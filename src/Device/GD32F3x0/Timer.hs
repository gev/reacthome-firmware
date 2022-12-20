{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}

module Device.GD32F3x0.Timer where

import           Include
import           Initialize
import qualified Interface.Counter as I
import qualified Interface.Timer               as I
import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Stdlib
import           Support.Device.GD32F3x0
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.Timer


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
  include = const $ inclRCU <> inclTimer

instance Initialize Timer where
  initialize (Timer {timer, rcu, param}) = [
      proc (show timer <> "_init") $ body $ do
        enablePeriphClock rcu
        deinitTimer       timer
        initTimer         timer param
        enableTimer       timer
    ]


instance I.Counter Timer where
  readCounter = readCounter . timer




instance Include (I.HandleTimer Timer) where
  include (I.HandleTimer (Timer {timer}) handle) =
    inclG >> inclMisc >>
    makeIRQHandler timer (handleIRQ timer handle)

instance Initialize (I.HandleTimer Timer) where
  initialize (I.HandleTimer {I.timer = Timer {timer, irq}}) = [
      proc (show timer <> "_irq_init") $ body $ do
        enableIrqNvic irq 0 0
        enableTimerInterrupt timer TIMER_INT_UP
    ]


handleIRQ :: TIMER_PERIPH -> Ivory eff () -> Ivory eff ()
handleIRQ timer handle = do
  flag <- getTimerInterruptFlag timer TIMER_INT_FLAG_UP
  when flag $ clearTimerInterruptFlag timer TIMER_INT_FLAG_UP
  handle
