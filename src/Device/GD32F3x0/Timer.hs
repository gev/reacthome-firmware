{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}

module Device.GD32F3x0.Timer where

import qualified Interface                     as I
import qualified Interface.IRQ                 as I
import qualified Interface.Timer               as I
import           Ivory.Language
import           Ivory.Stdlib
import           Support.Device.GD32F3x0
import           Support.Device.GD32F3x0.Misc  as S
import           Support.Device.GD32F3x0.RCU   as S
import           Support.Device.GD32F3x0.Timer as S


data Timer = Timer
  { timer :: TIMER_PERIPH
  , rcu   :: RCU_PERIPH
  , irq   :: IRQn
  , param :: TIMER_PARAM
  }

timer_2 = Timer TIMER2 RCU_TIMER2 TIMER2_IRQn

instance I.Interface Timer where

  dependencies = const [inclRCU, inclTimer]

  initialize (Timer {timer, rcu, param}) = do
    enablePeriphClock rcu
    deinitTimer       timer
    initTimer         timer param
    enableTimer       timer


instance I.Timer Timer

instance I.IRQ Timer where

  dependencies t handle = [inclG, inclMisc, incl $ makeHandler (timer t) handle]

  initialize t = do
    enableIrqNvic (irq t) 0 0
    I.enable t

  enable t = enableTimerInterrupt    (timer t) TIMER_INT_UP

makeHandler :: TIMER_PERIPH
            -> (forall s . Ivory (ProcEffects s ()) ())
            -> Def ('[] :-> ())
makeHandler t h = makeTimerHandler t $ handleIRQ h

handleIRQ :: (forall s. Ivory (ProcEffects s ()) ())
          -> TIMER_PERIPH
          -> (forall s. Ivory (ProcEffects s ()) ())
handleIRQ h t = do
    flag <- getTimerInterruptFlag t TIMER_INT_FLAG_UP
    when flag $ clearTimerInterruptFlag t TIMER_INT_FLAG_UP
    h
