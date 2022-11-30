module Device.GD32F3x0.USART where


import           Device.GD32F3x0.GPIO
import qualified Interface                     as I
import qualified Interface.USART               as I
import           Support.Device.GD32F3x0.GPIO  as S
import           Support.Device.GD32F3x0.RCU   as S
import           Support.Device.GD32F3x0.USART as S


data USART = USART
  { usart :: USART_PERIPH
  , rcu   :: RCU_PERIPH
  , rx    :: PORT
  , tx    :: PORT
  }

usart_1 = USART USART1
                RCU_USART1
                (pa_2 $ AF GPIO_AF_1)
                (pa_3 $ AF GPIO_AF_1)


instance I.Interface USART where
  dependecies = const $ inclUSART : dependecies'
  initialize (USART usart rcu rx tx) = do
    initialize' rx
    initialize' tx
    enablePeriphClock rcu
    deinitUSART     usart
    configReceive   usart USART_RECEIVE_ENABLE
    configTransmit  usart USART_TRANSMIT_ENABLE
    enableUSART     usart


instance I.USART USART
