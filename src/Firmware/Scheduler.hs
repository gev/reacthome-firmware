{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Firmware.Scheduler (scheduler) where

import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Stdlib
import           Support.Device.GD32F3x0
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.Timer

handle :: TIMER_PERIPH -> Def ('[] :-> ())
handle = makeTimerHandler $ \timer -> do
    flag <- getTimerInterruptFlag timer TIMER_INT_FLAG_UP
    when flag $ clearTimerInterruptFlag timer TIMER_INT_FLAG_UP


scheduler :: ModuleM ()
scheduler = do
  inclG
  inclRCU
  inclMisc
  inclTimer
  incl main
  incl $ handle TIMER2


main :: Def ('[] :-> Sint32)
main = proc "main" $ body $ do
  enableIrqNvic           TIMER2_IRQn 0 0
  enablePeriphClock       RCU_TIMER2
  deinitTimer             TIMER2
  initTimer               TIMER2  timerParam  { prescaler = 8399
                                              , period    = 9
                                              }
  clearTimerInterruptFlag TIMER2  TIMER_INT_FLAG_UP
  enableTimerInterrupt    TIMER2  TIMER_INT_UP
  enableTimer             TIMER2
  ret 0
