{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Device.GD32F4xx.GPIO where

import           Core.Context
import           Ivory.Language
import           Ivory.Support
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

pc_0  = pc gpio_pin_0
pc_1  = pc gpio_pin_1
pc_2  = pc gpio_pin_2
pc_3  = pc gpio_pin_3
pc_4  = pc gpio_pin_4
pc_5  = pc gpio_pin_5
pc_6  = pc gpio_pin_6
pc_7  = pc gpio_pin_7
pc_8  = pc gpio_pin_8
pc_9  = pc gpio_pin_9
pc_10 = pc gpio_pin_10
pc_11 = pc gpio_pin_11
pc_12 = pc gpio_pin_12
pc_13 = pc gpio_pin_13
pc_14 = pc gpio_pin_14
pc_15 = pc gpio_pin_15

pd_0  = pd gpio_pin_0
pd_1  = pd gpio_pin_1
pd_2  = pd gpio_pin_2
pd_3  = pd gpio_pin_3
pd_4  = pd gpio_pin_4
pd_5  = pd gpio_pin_5
pd_6  = pd gpio_pin_6
pd_7  = pd gpio_pin_7
pd_8  = pd gpio_pin_8
pd_9  = pd gpio_pin_9
pd_10 = pd gpio_pin_10
pd_11 = pd gpio_pin_11
pd_12 = pd gpio_pin_12
pd_13 = pd gpio_pin_13
pd_14 = pd gpio_pin_14
pd_15 = pd gpio_pin_15

pe_0  = pe gpio_pin_0
pe_1  = pe gpio_pin_1
pe_2  = pe gpio_pin_2
pe_3  = pe gpio_pin_3
pe_4  = pe gpio_pin_4
pe_5  = pe gpio_pin_5
pe_6  = pe gpio_pin_6
pe_7  = pe gpio_pin_7
pe_8  = pe gpio_pin_8
pe_9  = pe gpio_pin_9
pe_10 = pe gpio_pin_10
pe_11 = pe gpio_pin_11
pe_12 = pe gpio_pin_12
pe_13 = pe gpio_pin_13
pe_14 = pe gpio_pin_14
pe_15 = pe gpio_pin_15



pa :: GPIO_PIN -> MODE -> Port
pa = Port rcu_gpioa gpioa

pb :: GPIO_PIN -> MODE -> Port
pb = Port rcu_gpiob gpiob

pc :: GPIO_PIN -> MODE -> Port
pc = Port rcu_gpioc gpioc

pd :: GPIO_PIN -> MODE -> Port
pd = Port rcu_gpiod gpiod

pe :: GPIO_PIN -> MODE -> Port
pe = Port rcu_gpioe gpioe




io :: GPIO_MODE -> (MODE -> Port) -> Port
io m p = p $ MF m



initPort :: Port -> Def ('[] ':-> ())
initPort Port{..} = proc (symbol gpio <> "_" <> symbol pin <> "_init") $ body $ do
    enablePeriphClock rcu
    setOutputOptions gpio gpio_otype_pp gpio_ospeed_50mhz pin
    case mode of
        (MF mode) -> setMode gpio mode gpio_pupd_none pin
        (AF mode) -> setMode gpio gpio_mode_af gpio_pupd_none pin
                  >> setAF gpio mode pin
