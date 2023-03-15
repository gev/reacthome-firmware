{-# LANGUAGE RecordWildCards #-}

module Device.GD32F4xx.GPIO.Output where

import           Core.Context
import           Device.GD32F4xx.GPIO
import qualified Interface.GPIO.Output        as I
import           Ivory.Language.Module
import           Support.Device.GD32F4xx.GPIO as S



newtype Output = Output {getOutput :: Port}



output :: (MODE -> Port) -> Output
output = Output . io GPIO_MODE_OUTPUT



instance Include Output where
    include = include . getOutput

instance I.Output Output where
    set   (Output (Port {..})) = S.setBit   gpio pin
    reset (Output (Port {..})) = S.resetBit gpio pin
