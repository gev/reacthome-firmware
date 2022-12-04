{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}

module Device.GD32F3x0.GPIO where

import           Interface
import qualified Interface.GPIO               as I
import           Ivory.Language
import           Ivory.Language.Module
import           Support.Device.GD32F3x0.GPIO as S
import           Support.Device.GD32F3x0.RCU


newtype IN  = IN  PORT

newtype OUT = OUT PORT

data PORT = PORT
  { rcu  :: RCU_PERIPH
  , gpio :: GPIO_PERIPH
  , pin  :: GPIO_PIN
  , mode :: MODE
  }

data MODE
  = MF  GPIO_MODE
  | AF  GPIO_AF


in_pa_15  = input  pa_15
out_pa_15 = output pa_15

pa_2  = pa GPIO_PIN_2
pa_3  = pa GPIO_PIN_3
pa_15 = pa GPIO_PIN_15

pa = PORT RCU_GPIOA GPIOA


input :: (MODE -> PORT) -> IN
input = IN . io GPIO_MODE_INPUT

output :: (MODE -> PORT) -> OUT
output = OUT . io GPIO_MODE_OUTPUT

io :: GPIO_MODE -> (MODE -> PORT) -> PORT
io m p = p $ MF m


instance Interface IN where
  dependencies = const dependencies'
  initialize (IN p) = [initialize' p]

instance Interface OUT where
  dependencies = const dependencies'
  initialize (OUT p) = [initialize' p]


instance I.IN IN where
  get = undefined

instance I.OUT OUT where
  set   (OUT (PORT {gpio, pin})) = S.setBit gpio pin
  reset (OUT (PORT {gpio, pin})) = S.resetBit gpio pin


dependencies' :: [ModuleM ()]
dependencies' =  [inclRCU, inclGPIO]

initialize' :: PORT -> Def ('[] ':-> ())
initialize' (PORT {rcu, gpio, pin, mode}) =
    proc (show gpio <> "_" <> show pin <>"_init") $ body $ do
      enablePeriphClock rcu
      setOutputOptions  gpio GPIO_OTYPE_PP GPIO_OSPEED_50MHZ pin
      case mode of
        (MF mode) -> setMode gpio mode GPIO_PUPD_NONE pin
        (AF mode) -> setMode gpio GPIO_MODE_AF GPIO_PUPD_NONE pin
                  >> setAF gpio mode pin
