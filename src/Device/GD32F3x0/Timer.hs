{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}

module Device.GD32F3x0.Timer where

import           Device.GD32F3x0.IRQ           as D
import           Interface
import           Interface.IRQ                 as I
import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Stdlib
import           Support.Device.GD32F3x0       as S
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.Timer


data Timer = Timer
  { timer :: TIMER_PERIPH
  , rcu   :: RCU_PERIPH
  , param :: TIMER_PARAM
  }

timer_2 :: TIMER_PARAM -> Timer
timer_2 = Timer TIMER2 RCU_TIMER2

timer_2_irq :: TIMER_PARAM -> D.IRQ Timer
timer_2_irq = IRQ TIMER2_IRQn . timer_2


instance Interface Timer where

  dependencies = const [inclRCU, inclTimer]

  initialize (Timer {timer, rcu, param}) = [
      proc (show timer <> "_init") $ body $ do
        enablePeriphClock rcu
        deinitTimer       timer
        initTimer         timer param
        enableTimer       timer
    ]


instance Interface (D.IRQ Timer) where

  dependencies (IRQ {source}) =
    dependencies source <> [inclG,  inclMisc]

  initialize q@(IRQ {source, irq}) =
    initialize source <> [
      proc (show (timer source) <> "_irq_init") $ body $ do
        enableIrqNvic irq 0 0
        enable q
    ]

instance I.IRQ (D.IRQ Timer) where

  handleIRQ (IRQ {source = (Timer {timer})}) handle =
    makeIRQHandler timer $ do
      flag <- getTimerInterruptFlag timer TIMER_INT_FLAG_UP
      when flag $ clearTimerInterruptFlag timer TIMER_INT_FLAG_UP
      handle

  enable (IRQ {source}) =
    enableTimerInterrupt (timer source) TIMER_INT_UP
