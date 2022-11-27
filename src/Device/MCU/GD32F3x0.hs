module Device.MCU.GD32F3x0 where

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


pa :: GPIO_PIN -> MODE -> PORT
pa = PORT RCU_GPIOA GPIOA

io :: GPIO_MODE -> (MODE -> PORT) -> MCU_GPIO
io m p = PIO . p $ MF m

input :: (MODE -> PORT) -> MCU_GPIO
input = io GPIO_MODE_INPUT

output :: (MODE -> PORT) -> MCU_GPIO
output = io GPIO_MODE_OUTPUT

usart :: USART_PERIPH -> RCU_PERIPH -> GPIO_AF -> (MODE -> PORT) -> MCU_GPIO
usart u r m p  = USART u r . p $ AF m

pa_2 = pa GPIO_PIN_2
pa_3 = pa GPIO_PIN_3
pa_15 :: MODE -> PORT
pa_15 = pa GPIO_PIN_15

pa_15_out = output pa_15
pa_15_in  = input pa_15


usart_1 = usart USART1 RCU_USART1 GPIO_AF_1

usart_1_rx = usart_1 pa_2
usart_1_tx = usart_1 pa_3
