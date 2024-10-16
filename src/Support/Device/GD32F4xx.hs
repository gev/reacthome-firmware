module Support.Device.GD32F4xx where

import           Ivory.Language.Module
import           Support.CMSIS.CoreCM4
import           Support.CMSIS.CoreCMFunc
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
import           Support.Device.GD32F4xx.ENET
import           Support.Device.GD32F4xx.FMC
import           Support.Device.GD32F4xx.FmcOperation.FmcOperation


inclGD32F4xx :: ModuleDef
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
    inclENET
    inclFmcOperation
    inclFMC
    inclCoreCM4
    inclCoreCMFunc
