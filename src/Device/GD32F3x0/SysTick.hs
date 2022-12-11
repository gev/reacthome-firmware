module Device.GD32F3x0.SysTick where

import           Interface
import           Interface.IRQ
import           Ivory.Language
import           Ivory.Language.Module
import           Support.CMSIS.CoreCM4


newtype SysTick = SysTick Uint32

sysTick = SysTick


instance Interface SysTick where

  dependencies = const [inclCoreCM4]

  initialize (SysTick ticks) = [
      proc "systick_init" $ body $ sysTickConfig ticks
    ]


instance IRQ SysTick where

  handleIRQ _ handle =
    incl $ proc "SysTick_Handler" $ body handle

  enable _ = pure ()
