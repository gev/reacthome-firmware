module Device.MCU.GD32F3x0.USART where

import qualified Device.GPIO                   as D
import           Device.MCU.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.GPIO  as S
import           Support.Device.GD32F3x0.RCU   as S
import           Support.Device.GD32F3x0.USART as S


data USART = USART USART_PERIPH RCU_PERIPH PORT PORT

usart_1 = usart USART1 RCU_USART1 pa_2 pa_3 GPIO_AF_1

usart :: USART_PERIPH -> RCU_PERIPH -> (MODE -> PORT) -> (MODE -> PORT) -> GPIO_AF -> USART
usart u r rx tx m  = USART u r  (rx $ AF m) (tx $ AF m)


instance D.GPIO USART where
  dependecies = undefined
  initialize = undefined

instance D.USART USART
