{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Device.GD32F3x0.GPIO where

import           Core.Context
import           Data.Record
import           Ivory.Language
import           Ivory.Support                (ExtSymbol (symbol))
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.RCU



data Port = Port
    { rcu  :: RCU_PERIPH
    , gpio :: GPIO_PERIPH
    , pin  :: GPIO_PIN
    , mode :: MODE
    }


data MODE
    = MF GPIO_MODE
    | AF GPIO_AF



pa_0  = pa gpio_pin_0
pa_1  = pa gpio_pin_1
pa_2  = pa gpio_pin_2
pa_3  = pa gpio_pin_3
pa_4  = pa gpio_pin_4
pa_5  = pa gpio_pin_5
pa_6  = pa gpio_pin_6
pa_7  = pa gpio_pin_7
pa_8  = pa gpio_pin_8
pa_9  = pa gpio_pin_9
pa_10 = pa gpio_pin_10
pa_11 = pa gpio_pin_11
pa_12 = pa gpio_pin_12
pa_13 = pa gpio_pin_13
pa_14 = pa gpio_pin_14
pa_15 = pa gpio_pin_15

pb_0  = pb gpio_pin_0
pb_1  = pb gpio_pin_1
pb_2  = pb gpio_pin_2
pb_3  = pb gpio_pin_3
pb_4  = pb gpio_pin_4
pb_5  = pb gpio_pin_5
pb_6  = pb gpio_pin_6
pb_7  = pb gpio_pin_7
pb_8  = pb gpio_pin_8
pb_9  = pb gpio_pin_9
pb_10 = pb gpio_pin_10
pb_11 = pb gpio_pin_11
pb_12 = pb gpio_pin_12
pb_13 = pb gpio_pin_13
pb_14 = pb gpio_pin_14
pb_15 = pb gpio_pin_15



pa :: GPIO_PIN -> MODE -> Port
pa = Port rcu_gpioa gpioa

pb :: GPIO_PIN -> MODE -> Port
pb = Port rcu_gpiob gpiob



io :: GPIO_MODE -> (MODE -> Port) -> Port
io m p = p $ MF m



initPort :: Port -> Def ('[] ':-> ())
initPort p@Port{..} = proc (show p <>"_init") $ body $ do
    enablePeriphClock rcu
    setOutputOptions gpio gpio_otype_pp gpio_ospeed_50mhz pin
    case mode of
        (MF mode) -> setMode gpio mode gpio_pupd_none pin
        (AF mode) -> setMode gpio gpio_mode_af gpio_pupd_none pin
                  >> setAF gpio mode pin

instance Show Port where
    show Port{..} = symbol gpio <> "_" <> symbol pin <>"_init"
