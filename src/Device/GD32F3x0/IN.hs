module Device.GD32F3x0.IN where

import           Device.MCU.GD32F3x0.GPIO
import qualified Interface.GPIO               as I
import           Support.Device.GD32F3x0.GPIO as S


newtype IN = IN PORT

in_pa_15  = input pa_15

input :: (MODE -> PORT) -> IN
input = IN . io GPIO_MODE_INPUT


instance D.GPIO IN where
  dependecies = dependecies'
  initialize (IN p) = initialize' p

instance D.IN IN where
  get = undefined
