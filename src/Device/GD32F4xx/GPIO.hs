{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Device.GD32F4xx.GPIO where

import           Core.Context
import           Ivory.Language
import           Support.Device.GD32F4xx.GPIO
import           Support.Device.GD32F4xx.RCU



data Port = Port
    { rcu  :: RCU_PERIPH
    , gpio :: GPIO_PERIPH
    , pin  :: GPIO_PIN
    , mode :: MODE
    }

data MODE
    = MF GPIO_MODE
    | AF GPIO_AF



pa_0  = pa GPIO_PIN_0
pa_1  = pa GPIO_PIN_1
pa_2  = pa GPIO_PIN_2
pa_3  = pa GPIO_PIN_3
pa_4  = pa GPIO_PIN_4
pa_5  = pa GPIO_PIN_5
pa_6  = pa GPIO_PIN_6
pa_7  = pa GPIO_PIN_7
pa_8  = pa GPIO_PIN_8
pa_9  = pa GPIO_PIN_9
pa_10 = pa GPIO_PIN_10
pa_11 = pa GPIO_PIN_11
pa_12 = pa GPIO_PIN_12
pa_13 = pa GPIO_PIN_13
pa_14 = pa GPIO_PIN_14
pa_15 = pa GPIO_PIN_15

pb_0  = pb GPIO_PIN_0
pb_1  = pb GPIO_PIN_1
pb_2  = pb GPIO_PIN_2
pb_3  = pb GPIO_PIN_3
pb_4  = pb GPIO_PIN_4
pb_5  = pb GPIO_PIN_5
pb_6  = pb GPIO_PIN_6
pb_7  = pb GPIO_PIN_7
pb_8  = pb GPIO_PIN_8
pb_9  = pb GPIO_PIN_9
pb_10 = pb GPIO_PIN_10
pb_11 = pb GPIO_PIN_11
pb_12 = pb GPIO_PIN_12
pb_13 = pb GPIO_PIN_13
pb_14 = pb GPIO_PIN_14
pb_15 = pb GPIO_PIN_15



pa :: GPIO_PIN -> MODE -> Port
pa = Port RCU_GPIOA GPIOA

pb :: GPIO_PIN -> MODE -> Port
pb = Port RCU_GPIOB GPIOB



io :: GPIO_MODE -> (MODE -> Port) -> Port
io m p = p $ MF m



initPort :: Port -> Def ('[] ':-> ())
initPort (Port {..}) = proc (show gpio <> "_" <> show pin <>"_init") $ body $ do
    enablePeriphClock rcu
    setOutputOptions gpio GPIO_OTYPE_PP GPIO_OSPEED_50MHZ pin
    case mode of
        (MF mode) -> setMode gpio mode GPIO_PUPD_NONE pin
        (AF mode) -> setMode gpio GPIO_MODE_AF GPIO_PUPD_NONE pin
                  >> setAF gpio mode pin
