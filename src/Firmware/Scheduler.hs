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
