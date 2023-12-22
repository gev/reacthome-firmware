{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Device.GD32F3x0.GPIO.Mode  where

import           Support.Device.GD32F3x0.GPIO



data Mode
    = MF GPIO_MODE GPIO_OTYPE GPIO_PUPD
    | AF GPIO_AF



input :: GPIO_PUPD -> Mode
input = MF gpio_mode_input gpio_otype_pp

output :: Mode
output = MF gpio_mode_output gpio_otype_pp gpio_pupd_none

openDrain :: Mode
openDrain = MF gpio_mode_output gpio_otype_od gpio_pupd_none



af_0 :: Mode
af_0 = AF gpio_af_0

af_1 :: Mode
af_1 = AF gpio_af_1

af_2 :: Mode
af_2 = AF gpio_af_2
