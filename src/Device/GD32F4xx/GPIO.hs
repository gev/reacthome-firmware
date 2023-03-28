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

pc_0  = pc GPIO_PIN_0
pc_1  = pc GPIO_PIN_1
pc_2  = pc GPIO_PIN_2
pc_3  = pc GPIO_PIN_3
pc_4  = pc GPIO_PIN_4
pc_5  = pc GPIO_PIN_5
pc_6  = pc GPIO_PIN_6
pc_7  = pc GPIO_PIN_7
pc_8  = pc GPIO_PIN_8
pc_9  = pc GPIO_PIN_9
pc_10 = pc GPIO_PIN_10
pc_11 = pc GPIO_PIN_11
pc_12 = pc GPIO_PIN_12
pc_13 = pc GPIO_PIN_13
pc_14 = pc GPIO_PIN_14
pc_15 = pc GPIO_PIN_15

pd_0  = pd GPIO_PIN_0
pd_1  = pd GPIO_PIN_1
pd_2  = pd GPIO_PIN_2
pd_3  = pd GPIO_PIN_3
pd_4  = pd GPIO_PIN_4
pd_5  = pd GPIO_PIN_5
pd_6  = pd GPIO_PIN_6
pd_7  = pd GPIO_PIN_7
pd_8  = pd GPIO_PIN_8
pd_9  = pd GPIO_PIN_9
pd_10 = pd GPIO_PIN_10
pd_11 = pd GPIO_PIN_11
pd_12 = pd GPIO_PIN_12
pd_13 = pd GPIO_PIN_13
pd_14 = pd GPIO_PIN_14
pd_15 = pd GPIO_PIN_15

pe_0  = pe GPIO_PIN_0
pe_1  = pe GPIO_PIN_1
pe_2  = pe GPIO_PIN_2
pe_3  = pe GPIO_PIN_3
pe_4  = pe GPIO_PIN_4
pe_5  = pe GPIO_PIN_5
pe_6  = pe GPIO_PIN_6
pe_7  = pe GPIO_PIN_7
pe_8  = pe GPIO_PIN_8
pe_9  = pe GPIO_PIN_9
pe_10 = pe GPIO_PIN_10
pe_11 = pe GPIO_PIN_11
pe_12 = pe GPIO_PIN_12
pe_13 = pe GPIO_PIN_13
pe_14 = pe GPIO_PIN_14
pe_15 = pe GPIO_PIN_15



pa :: GPIO_PIN -> MODE -> Port
pa = Port RCU_GPIOA GPIOA

pb :: GPIO_PIN -> MODE -> Port
pb = Port RCU_GPIOB GPIOB

pc :: GPIO_PIN -> MODE -> Port
pc = Port RCU_GPIOC GPIOC

pd :: GPIO_PIN -> MODE -> Port
pd = Port RCU_GPIOD GPIOD

pe :: GPIO_PIN -> MODE -> Port
pe = Port RCU_GPIOE GPIOE




io :: GPIO_MODE -> (MODE -> Port) -> Port
io m p = p $ MF m



initPort :: Port -> Def ('[] ':-> ())
initPort Port{..} = proc (show gpio <> "_" <> show pin <>"_init") $ body $ do
    enablePeriphClock rcu
    setOutputOptions gpio GPIO_OTYPE_PP GPIO_OSPEED_50MHZ pin
    case mode of
        (MF mode) -> setMode gpio mode GPIO_PUPD_NONE pin
        (AF mode) -> setMode gpio GPIO_MODE_AF GPIO_PUPD_NONE pin
                  >> setAF gpio mode pin
