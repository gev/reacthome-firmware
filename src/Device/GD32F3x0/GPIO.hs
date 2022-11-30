{-# LANGUAGE NamedFieldPuns #-}


module Device.GD32F3x0.GPIO where

import           Ivory.Language
import           Ivory.Language.Module
import           Support.Device.GD32F3x0.GPIO as S
import           Support.Device.GD32F3x0.RCU  as S


data PORT = PORT
  { rcu  :: RCU_PERIPH
  , gpio :: GPIO_PERIPH
  , pin  :: GPIO_PIN
  , mode :: MODE
  }

data MODE
  = MF  GPIO_MODE
  | AF  GPIO_AF


pa_2  = pa GPIO_PIN_2
pa_3  = pa GPIO_PIN_3
pa_15 = pa GPIO_PIN_15


io :: GPIO_MODE -> (MODE -> PORT) -> PORT
io m p = p $ MF m

pa :: GPIO_PIN -> MODE -> PORT
pa = PORT RCU_GPIOA GPIOA


dependecies' :: b -> [ModuleM ()]
dependecies' = const [inclRCU, inclGPIO]

initialize' :: PORT -> Ivory eff ()
initialize' (PORT {rcu, gpio, pin, mode = (MF mode)}) = do
  enablePeriphClock rcu
  setMode           gpio mode GPIO_PUPD_NONE pin
  setOutputOptions  gpio GPIO_OTYPE_PP GPIO_OSPEED_50MHZ pin
