module Device.GD32F3x0.GPIO.Input where

import           Core.Context
import           Device.GD32F3x0.GPIO
import qualified Interface.GPIO.Input         as I
import           Ivory.Language
import           Support.Device.GD32F3x0.GPIO



newtype Input  = Input  {getInput :: Port}



input :: (MODE -> Port) -> Input
input = Input . io GPIO_MODE_INPUT



instance Include Input where
    include = include . getInput

instance I.Input Input where
    get = undefined
