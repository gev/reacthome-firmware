{-# LANGUAGE NamedFieldPuns #-}

module Device.GD32F3x0.GPIO.Output where

import           Core.Include
import           Core.Initialize
import           Device.GD32F3x0.GPIO
import qualified Interface.GPIO.Output        as I
import           Ivory.Language.Module
import           Support.Device.GD32F3x0.GPIO as S



newtype Output = Output {getOutput :: Port}



output :: (MODE -> Port) -> Output
output = Output . io GPIO_MODE_OUTPUT



instance Include Output where
    include = include . getOutput

instance Initialize Output where
    initialize = initialize . getOutput

instance I.Output Output where
    set   (Output (Port {gpio, pin})) = S.setBit   gpio pin
    reset (Output (Port {gpio, pin})) = S.resetBit gpio pin