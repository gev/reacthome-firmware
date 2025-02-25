module Support.Device.GD32F3x0 where

import           Ivory.Language.Module
import           Support.CMSIS.CoreCM4
import           Support.CMSIS.CoreCMFunc
import           Support.Device.GD32F3x0.ADC
import           Support.Device.GD32F3x0.DAC
import           Support.Device.GD32F3x0.DBG
import           Support.Device.GD32F3x0.DMA
import           Support.Device.GD32F3x0.EXTI
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.SYSCFG
import           Support.Device.GD32F3x0.System
import           Support.Device.GD32F3x0.Timer
import           Support.Device.GD32F3x0.USART
import           Support.Device.GD32F3x0.FMC
import           Support.Device.GD32F3x0.I2C


inclGD32F3x0 :: ModuleDef
inclGD32F3x0 = do
    inclADC
    inclDAC
    inclDBG
    inclDMA
    inclExti
    inclGPIO
    inclIRQ
    inclMisc
    inclRCU
    inclSYSCFG
    inclSystem
    inclTimer
    inclUSART
    inclI2C
    inclFMC
    inclCoreCM4
    inclCoreCMFunc
