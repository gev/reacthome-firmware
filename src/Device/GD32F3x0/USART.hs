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

  dependencies = const $ inclUSART : dependencies'

  initialize (USART usart rcu rx tx) = do
    initialize' rx
    initialize' tx
    enablePeriphClock rcu
    deinitUSART     usart
    configReceive   usart USART_RECEIVE_ENABLE
    configTransmit  usart USART_TRANSMIT_ENABLE
    enableUSART     usart


instance I.USART USART where

  setBaudrate u = S.setBaudrate $ usart u

  setWordLength u wl = S.setWordLength (usart u) (coerceWordLength wl)

  setStopBit u sb = S.setStopBit (usart u) (coerceStopBit sb)

  setParity u p = S.configParity (usart u) (coerceParity p)

  receiveData u = S.receiveData $ usart u

  transmitData u = S.transmitData $ usart u


{-
  TODO: add all values of word length, stopbit and parity
-}
coerceWordLength :: I.WordLength -> USART_WORD_LENGTH
coerceWordLength I.WL_8b = USART_WL_8BIT

coerceStopBit :: I.StopBit -> USART_STOP_BIT
coerceStopBit I.SB_1b = USART_STB_1BIT

coerceParity :: I.Parity -> USART_PARITY_CFG
coerceParity I.None = USART_PM_NONE
