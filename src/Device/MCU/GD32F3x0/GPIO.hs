{-# LANGUAGE NamedFieldPuns #-}

module Device.MCU.GD32F3x0.GPIO where

import qualified Device.GPIO                   as D

import           Support.Device.GD32F3x0.GPIO  as S
import           Support.Device.GD32F3x0.RCU   as S
import           Support.Device.GD32F3x0.USART as S

newtype IN = IN PORT
newtype OUT = OUT PORT

data USART = USART USART_PERIPH RCU_PERIPH PORT PORT

data PORT = PORT
  { rcu  :: RCU_PERIPH
  , gpio :: GPIO_PERIPH
  , pin  :: GPIO_PIN
  , mode :: MODE
  }

data MODE
  = MF  GPIO_MODE
  | AF  GPIO_AF


in_15  = input  pa_15
out_15 = output pa_15

usart_1 = usart USART1 RCU_USART1 pa_2 pa_3 GPIO_AF_1


io :: GPIO_MODE -> (MODE -> PORT) -> PORT
io m p = p $ MF m

input :: (MODE -> PORT) -> IN
input = IN . io GPIO_MODE_INPUT

output :: (MODE -> PORT) -> OUT
output = OUT . io GPIO_MODE_OUTPUT

usart :: USART_PERIPH -> RCU_PERIPH -> (MODE -> PORT) -> (MODE -> PORT) -> GPIO_AF -> USART
usart u r rx tx m  = USART u r  (rx $ AF m) (tx $ AF m)

pa :: GPIO_PIN -> MODE -> PORT
pa = PORT RCU_GPIOA GPIOA

pa_2  = pa GPIO_PIN_2
pa_3  = pa GPIO_PIN_3
pa_15 = pa GPIO_PIN_15


instance D.GPIO OUT where
  dependecies = const [inclRCU, inclGPIO]
  initialize (OUT (PORT {rcu, gpio, pin, mode = (MF mode)})) = do
    enablePeriphClock rcu
    setMode           gpio mode GPIO_PUPD_NONE pin
    setOutputOptions  gpio GPIO_OTYPE_PP GPIO_OSPEED_50MHZ pin

instance D.OUT OUT where
  set   (OUT (PORT {gpio, pin})) = S.setBit gpio pin
  reset (OUT (PORT {gpio, pin})) = S.resetBit gpio pin


instance D.GPIO USART where
  dependecies = undefined
  initialize = undefined

instance D.USART USART
