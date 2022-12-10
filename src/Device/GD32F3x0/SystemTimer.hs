module Device.GD32F3x0.SystemTimer where

import           Interface
import           Interface.IRQ
import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Stdlib
import           Support.CMSIS.CoreCM4


newtype SystemTimer = SystemTimer Uint32

systemTimer = SystemTimer


instance Interface SystemTimer where

  dependencies = const [inclCoreCM4]

  initialize (SystemTimer ticks) = [
      proc "system_timer_init" $ body $ sysTickConfig ticks
    ]


instance IRQ SystemTimer where

  handleIRQ _ handle =
    incl $ proc "SysTick_Handler" $ body handle

  enable _ = pure ()
