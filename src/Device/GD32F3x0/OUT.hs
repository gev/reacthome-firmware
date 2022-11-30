{-# LANGUAGE NamedFieldPuns #-}

module Device.GD32F3x0.OUT where

import           Device.GD32F3x0.GPIO
import qualified Interface.GPIO               as I
import           Support.Device.GD32F3x0.GPIO as S


newtype OUT = OUT PORT

out_pa_15  = output  pa_15

output :: (MODE -> PORT) -> OUT
output = OUT . io GPIO_MODE_OUTPUT


instance I.GPIO OUT where
  dependecies = dependecies'
  initialize (OUT p) = initialize' p

instance I.OUT OUT where
  set   (OUT (PORT {gpio, pin})) = S.setBit gpio pin
  reset (OUT (PORT {gpio, pin})) = S.resetBit gpio pin
