module Device.GD32F3x0.SystemTimer where

import           Interface
import           Interface.IRQ
import           Interface.Timer
import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Stdlib
import           Support.CMSIS.CoreCM4
import           Support.CMSIS.DWT


newtype SystemTimer = SystemTimer Uint32

systemTimer = SystemTimer 84000


instance Interface SystemTimer where

  dependencies = const [inclCoreCM4, inclDWT]

  initialize (SystemTimer ticks) = [
      proc "system_timer_init" $ body
                               $ dwtDelayInit
                              >> sysTickConfig ticks
    ]


instance Timer SystemTimer where
  current _ = dwtCycleCounter
  delay   _ = dwtDelay


instance IRQ SystemTimer where

  handleIRQ _ handle =
    incl $ proc "SysTick_Handler" $ body handle

  enable _ = pure ()
