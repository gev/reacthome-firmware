{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Device.GD32F3x0.GPIO where

import           Core.Context
import           Data.Record
import           Ivory.Language
import           Ivory.Support
import           Support.Device.GD32F3x0.GPIO as S
import           Support.Device.GD32F3x0.RCU


type Port = "port_struct"

[ivory|
    struct port_struct
    { rcu  :: Stored RCU_PERIPH
    ; gpio :: Stored GPIO_PERIPH
    ; pin  :: Stored GPIO_PIN
    }
|]


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



pa :: GPIO_PIN  -> [InitStruct Port]
pa pin' = [ rcu .= ival rcu_gpioa
          , pin .= ival pin'
          ]

pb :: GPIO_PIN -> [InitStruct Port]
pb pin' = [ rcu .= ival rcu_gpiob
          , pin .= ival pin'
          ]



initGPIO :: Record Port -> MODE -> Ivory eff ()
initGPIO port mode = do
    rcu'  <- deref $ port ~> rcu
    gpio' <- deref $ port ~> gpio
    pin'  <- deref $ port ~> pin
    enablePeriphClock rcu'
    setOutputOptions gpio' gpio_otype_pp gpio_ospeed_50mhz pin'
    case mode of
        MF mode -> setMode gpio' mode gpio_pupd_none pin'
        AF mode -> setMode gpio' gpio_mode_af gpio_pupd_none pin'
                >> setAF gpio' mode pin'
