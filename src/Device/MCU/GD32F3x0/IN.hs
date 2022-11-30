module Device.MCU.GD32F3x0.IN where

import qualified Device.GPIO                  as D
import           Device.MCU.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.GPIO as S


newtype IN = IN PORT

in_15  = input pa_15

input :: (MODE -> PORT) -> IN
input = IN . io GPIO_MODE_INPUT


instance D.GPIO IN where
  dependecies = dependecies'
  initialize (IN p) = initialize' p

instance D.IN IN where
  getBit = undefined
