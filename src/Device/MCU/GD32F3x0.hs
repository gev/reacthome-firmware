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

usart :: USART_PERIPH -> RCU_PERIPH -> (MODE -> PORT) -> MCU_GPIO
usart u r p = USART u r . p $ AF GPIO_AF_1

pa_2 = pa GPIO_PIN_2
pa_3 = pa GPIO_PIN_3
pa_15 = pa GPIO_PIN_15

pa_15_out = PIO . pa_15 $ MF GPIO_MODE_OUTPUT
pa_15_in  = PIO . pa_15 $ MF GPIO_MODE_INPUT


usart_1 = usart USART1 RCU_USART1

usart_1_rx = usart_1 pa_2
usart_1_tx = usart_1 pa_3

