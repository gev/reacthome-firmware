module Support.Device.GD32F3x0 where

import           Ivory.Language.Module
import           Support.CMSIS.CoreCM4
import           Support.Device.GD32F3x0.DBG
import           Support.Device.GD32F3x0.DMA
import           Support.Device.GD32F3x0.EXTI
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.SYSCFG
import           Support.Device.GD32F3x0.Timer
import           Support.Device.GD32F3x0.USART


inclGD32F3x0 :: ModuleM ()
inclGD32F3x0 = do
    inclDBG
    inclDMA
    inclExti
    inclGPIO
    inclIRQ
    inclMisc
    inclRCU
    inclSYSCFG
    inclTimer
    inclUSART
    inclCoreCM4
