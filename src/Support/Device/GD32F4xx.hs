module Support.Device.GD32F4xx where

import           Ivory.Language.Module
import           Support.CMSIS.CoreCM4
import           Support.Device.GD32F4xx.DBG
import           Support.Device.GD32F4xx.DMA
import           Support.Device.GD32F4xx.EXTI
import           Support.Device.GD32F4xx.GPIO
import           Support.Device.GD32F4xx.IRQ
import           Support.Device.GD32F4xx.Misc
import           Support.Device.GD32F4xx.RCU
import           Support.Device.GD32F4xx.SYSCFG
import           Support.Device.GD32F4xx.Timer
import           Support.Device.GD32F4xx.USART


inclGD32F4xx :: ModuleM ()
inclGD32F4xx = do
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
