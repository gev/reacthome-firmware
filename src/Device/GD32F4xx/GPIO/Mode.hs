module Device.GD32F4xx.GPIO.Mode where

import Support.Device.GD32F4xx.GPIO

data Mode
    = MF GPIO_MODE GPIO_OTYPE
    | AF GPIO_AF

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

af_3 :: Mode
af_3 = AF gpio_af_3

af_4 :: Mode
af_4 = AF gpio_af_4

af_5 :: Mode
af_5 = AF gpio_af_5

af_6 :: Mode
af_6 = AF gpio_af_6

af_7 :: Mode
af_7 = AF gpio_af_7

af_8 :: Mode
af_8 = AF gpio_af_8

af_9 :: Mode
af_9 = AF gpio_af_9

af_10 :: Mode
af_10 = AF gpio_af_10

af_11 :: Mode
af_11 = AF gpio_af_11

af_12 :: Mode
af_12 = AF gpio_af_12

af_13 :: Mode
af_13 = AF gpio_af_13

af_14 :: Mode
af_14 = AF gpio_af_14

af_15 :: Mode
af_15 = AF gpio_af_15
