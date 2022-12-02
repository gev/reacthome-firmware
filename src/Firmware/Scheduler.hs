{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Firmware.Scheduler (scheduler) where

import           Device.GD32F3x0.Timer
import           Feature
import           Feature.Scheduler
import           Support.Device.GD32F3x0.Timer


scheduler = [Feature $ Scheduler 1
                     $ timer_2 timerParam { prescaler = 8399
                                          , period    = 9
                                          }
            ]
-- handle :: TIMER_PERIPH -> Def ('[] :-> ())
-- handle = makeTimerHandler $ \timer -> do
--     flag <- getTimerInterruptFlag timer TIMER_INT_FLAG_UP
--     when flag $ clearTimerInterruptFlag timer TIMER_INT_FLAG_UP


-- scheduler :: ModuleM ()
-- scheduler = do
--   inclG
--   inclRCU
--   inclMisc
--   inclTimer
--   incl main
--   incl $ handle TIMER2


-- main :: Def ('[] :-> Sint32)
-- main = proc "main" $ body $ do
--   enableIrqNvic           TIMER2_IRQn 0 0
--   enablePeriphClock       RCU_TIMER2
--   deinitTimer             TIMER2
--   initTimer               TIMER2  timerParam  { prescaler = 8399
--                                               , period    = 9
--                                               }
--   clearTimerInterruptFlag TIMER2  TIMER_INT_FLAG_UP
--   enableTimerInterrupt    TIMER2  TIMER_INT_UP
--   enableTimer             TIMER2
--   ret 0
