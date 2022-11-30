{-# LANGUAGE NamedFieldPuns #-}

module Device.MCU.GD32F3x0.OUT where

import qualified Device.GPIO                  as D
import           Device.MCU.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.GPIO as S


newtype OUT = OUT PORT

out_15  = output  pa_15

output :: (MODE -> PORT) -> OUT
output = OUT . io GPIO_MODE_OUTPUT


instance D.GPIO OUT where
  dependecies = dependecies'
  initialize (OUT p) = initialize' p

instance D.OUT OUT where
  set   (OUT (PORT {gpio, pin})) = S.setBit gpio pin
  reset (OUT (PORT {gpio, pin})) = S.resetBit gpio pin
