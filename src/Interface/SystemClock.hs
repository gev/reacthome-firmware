{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Interface.SystemClock where

import           Interface
import           Interface.IRQ
import           Interface.Timer



data SystemClock where
  SystemClock :: (IRQ q, Timer t)
              => { scheduleTimer :: q
                 , systemTimer   :: t
                 }
              -> SystemClock



instance Interface SystemClock where

  dependencies (SystemClock sch sys) =
    dependencies sch <> dependencies sys

  initialize (SystemClock sch sys) =
    initialize sch <> initialize sys



instance Timer SystemClock where
  readCounter (SystemClock {systemTimer}) = readCounter systemTimer



instance IRQ SystemClock where

  handleIRQ (SystemClock {scheduleTimer}) = handleIRQ scheduleTimer

  enable (SystemClock {scheduleTimer}) = enable scheduleTimer
