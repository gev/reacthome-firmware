{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Device.GD32F3x0.Timer where

import qualified Interface                     as I
import qualified Interface.IRQ                 as Q
import qualified Interface.Timer               as I
import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Stdlib
import qualified Support.Device.GD32F3x0       as S
import           Support.Device.GD32F3x0.Misc  as S
import           Support.Device.GD32F3x0.RCU   as S
import           Support.Device.GD32F3x0.Timer as S


data Timer = Timer
  { timer :: TIMER_PERIPH
  , rcu   :: RCU_PERIPH
  , param :: TIMER_PARAM
  }

data IRQn = IRQn
  { source :: Timer
  , irq    :: S.IRQn
  }

timer_2 :: TIMER_PARAM -> Timer
timer_2 = Timer TIMER2 RCU_TIMER2

timer_2_irq :: TIMER_PARAM -> IRQn
timer_2_irq p = IRQn (timer_2 p) S.TIMER2_IRQn


instance I.Interface Timer where

  dependencies = const [inclRCU, inclTimer]

  initialize (Timer {timer, rcu, param}) = [
      proc (show timer <> "_init") $ body $ do
        enablePeriphClock rcu
        deinitTimer       timer
        initTimer         timer param
        enableTimer       timer
    ]


instance I.Interface IRQn where

  dependencies (IRQn {source}) = I.dependencies source
                              <> [S.inclG,  inclMisc]

  initialize q@(IRQn {source, irq}) = I.initialize source <> [
      proc (show (timer source) <> "_irq_init") $ body $ do
        enableIrqNvic irq 0 0
        Q.enable q
    ]

instance Q.IRQ IRQn where
  handleIRQ (IRQn {source}) = makeHandler . timer $ source
  enable (IRQn {source}) = enableTimerInterrupt (timer source) TIMER_INT_UP

makeHandler :: TIMER_PERIPH
            -> (forall s . Ivory (ProcEffects s ()) ())
            -> ModuleM ()
makeHandler t h = S.makeIRQHandler t $ handleIRQ h

handleIRQ :: (forall s. Ivory (ProcEffects s ()) ())
          -> TIMER_PERIPH
          -> (forall s. Ivory (ProcEffects s ()) ())
handleIRQ h t = do
    flag <- getTimerInterruptFlag t TIMER_INT_FLAG_UP
    when flag $ clearTimerInterruptFlag t TIMER_INT_FLAG_UP
    h


instance I.Timer Timer
