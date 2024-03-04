{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Device.GD32F3x0.GPIO.Mode  where

import           Support.Device.GD32F3x0.GPIO



data Mode
    = MF GPIO_MODE GPIO_OTYPE
    | AF GPIO_AF
    | AN GPIO_MODE



input :: Mode
input = MF gpio_mode_input gpio_otype_pp

output :: Mode
output = MF gpio_mode_output gpio_otype_pp

openDrain :: Mode
openDrain = MF gpio_mode_output gpio_otype_od



af_0 :: Mode
af_0 = AF gpio_af_0

af_1 :: Mode
af_1 = AF gpio_af_1

af_2 :: Mode
af_2 = AF gpio_af_2

af_4 :: Mode
af_4 = AF gpio_af_4

af_5 :: Mode
af_5 = AF gpio_af_5


analog :: Mode
analog = AN gpio_mode_analog
