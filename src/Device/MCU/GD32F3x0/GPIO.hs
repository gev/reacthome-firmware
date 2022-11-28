module Device.MCU.GD32F3x0.GPIO
  ( MCU_GPIO
  , in_15
  , out_15
  , usart_1
  ) where

import           Device.GPIO

import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.USART


data MCU_GPIO
  = PIO PORT
  | USART USART_PERIPH RCU_PERIPH PORT

data PORT = PORT
  { rcu  :: RCU_PERIPH
  , gpio :: GPIO_PERIPH
  , pin  :: GPIO_PIN
  , mode :: MODE
  }

data MODE
  = MF  GPIO_MODE
  | AF  GPIO_AF


in_15  = IN  $ input  pa_15
out_15 = OUT $ output pa_15

usart_1 = UART
  { rx = use pa_2
  , tx = use pa_3
  }
  where use = usart USART1 RCU_USART1 GPIO_AF_1



io :: GPIO_MODE -> (MODE -> PORT) -> MCU_GPIO
io m p = PIO . p $ MF m

input :: (MODE -> PORT) -> MCU_GPIO
input = io GPIO_MODE_INPUT

output :: (MODE -> PORT) -> MCU_GPIO
output = io GPIO_MODE_OUTPUT

usart :: USART_PERIPH -> RCU_PERIPH -> GPIO_AF -> (MODE -> PORT) -> MCU_GPIO
usart u r m p  = USART u r . p $ AF m

pa :: GPIO_PIN -> MODE -> PORT
pa = PORT RCU_GPIOA GPIOA

pa_2  = pa GPIO_PIN_2
pa_3  = pa GPIO_PIN_3
pa_15 = pa GPIO_PIN_15
