{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.GPIO (
    GPIO_PERIPH,
    gpioa,
    gpiob,
    GPIO_MODE,
    gpio_mode_input,
    gpio_mode_output,
    gpio_mode_af,
    gpio_mode_analog,
    GPIO_PUPD,
    gpio_pupd_none,
    gpio_pupd_pullup,
    gpio_pupd_pulldown,
    GPIO_OTYPE,
    gpio_otype_pp,
    gpio_otype_od,
    GPIO_SPEED,
    gpio_ospeed_50mhz,
    GPIO_PIN,
    gpio_pin_0,
    gpio_pin_1,
    gpio_pin_2,
    gpio_pin_3,
    gpio_pin_4,
    gpio_pin_5,
    gpio_pin_6,
    gpio_pin_7,
    gpio_pin_8,
    gpio_pin_9,
    gpio_pin_10,
    gpio_pin_11,
    gpio_pin_12,
    gpio_pin_13,
    gpio_pin_14,
    gpio_pin_15,
    GPIO_AF,
    gpio_af_0,
    gpio_af_1,
    gpio_af_2,
    gpio_af_4,
    gpio_af_5,
    setMode,
    setOutputOptions,
    setBit,
    resetBit,
    toggleBit,
    setAF,
    getInputBit,
    getOutputBit,
    inclGPIO,
) where

import Ivory.Language hiding (setBit)
import Ivory.Support
import Ivory.Support.Device.GD32F3x0

newtype GPIO_PERIPH = GPIO_PERIPH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
instance ExtSymbol GPIO_PERIPH

gpioa = GPIO_PERIPH $ ext "GPIOA"
gpiob = GPIO_PERIPH $ ext "GPIOB"

newtype GPIO_MODE = GPIO_MODE Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

gpio_mode_input = GPIO_MODE $ ext "GPIO_MODE_INPUT"
gpio_mode_output = GPIO_MODE $ ext "GPIO_MODE_OUTPUT"
gpio_mode_af = GPIO_MODE $ ext "GPIO_MODE_AF"
gpio_mode_analog = GPIO_MODE $ ext "GPIO_MODE_ANALOG"

newtype GPIO_PUPD = GPIO_PUPD Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

gpio_pupd_none = GPIO_PUPD $ ext "GPIO_PUPD_NONE"
gpio_pupd_pullup = GPIO_PUPD $ ext "GPIO_PUPD_PULLUP"
gpio_pupd_pulldown = GPIO_PUPD $ ext "GPIO_PUPD_PULLDOWN"

newtype GPIO_SPEED = GPIO_SPEED Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

gpio_ospeed_50mhz = GPIO_SPEED $ ext "GPIO_OSPEED_50MHZ"

newtype GPIO_OTYPE = GPIO_OTYPE Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

gpio_otype_pp = GPIO_OTYPE $ ext "GPIO_OTYPE_PP"
gpio_otype_od = GPIO_OTYPE $ ext "GPIO_OTYPE_OD"

newtype GPIO_PIN = GPIO_PIN Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
instance ExtSymbol GPIO_PIN

gpio_pin_0 = GPIO_PIN $ ext "GPIO_PIN_0"
gpio_pin_1 = GPIO_PIN $ ext "GPIO_PIN_1"
gpio_pin_2 = GPIO_PIN $ ext "GPIO_PIN_2"
gpio_pin_3 = GPIO_PIN $ ext "GPIO_PIN_3"
gpio_pin_4 = GPIO_PIN $ ext "GPIO_PIN_4"
gpio_pin_5 = GPIO_PIN $ ext "GPIO_PIN_5"
gpio_pin_6 = GPIO_PIN $ ext "GPIO_PIN_6"
gpio_pin_7 = GPIO_PIN $ ext "GPIO_PIN_7"
gpio_pin_8 = GPIO_PIN $ ext "GPIO_PIN_8"
gpio_pin_9 = GPIO_PIN $ ext "GPIO_PIN_9"
gpio_pin_10 = GPIO_PIN $ ext "GPIO_PIN_10"
gpio_pin_11 = GPIO_PIN $ ext "GPIO_PIN_11"
gpio_pin_12 = GPIO_PIN $ ext "GPIO_PIN_12"
gpio_pin_13 = GPIO_PIN $ ext "GPIO_PIN_13"
gpio_pin_14 = GPIO_PIN $ ext "GPIO_PIN_14"
gpio_pin_15 = GPIO_PIN $ ext "GPIO_PIN_15"

newtype GPIO_AF = GPIO_AF Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

gpio_af_0 = GPIO_AF $ ext "GPIO_AF_0"
gpio_af_1 = GPIO_AF $ ext "GPIO_AF_1"
gpio_af_2 = GPIO_AF $ ext "GPIO_AF_2"
gpio_af_4 = GPIO_AF $ ext "GPIO_AF_4"
gpio_af_5 = GPIO_AF $ ext "GPIO_AF_5"

setMode :: GPIO_PERIPH -> GPIO_MODE -> GPIO_PUPD -> GPIO_PIN -> Ivory eff ()
setMode = call_ gpio_mode_set

gpio_mode_set :: Def ('[GPIO_PERIPH, GPIO_MODE, GPIO_PUPD, GPIO_PIN] :-> ())
gpio_mode_set = fun "gpio_mode_set"

setOutputOptions :: GPIO_PERIPH -> GPIO_OTYPE -> GPIO_SPEED -> GPIO_PIN -> Ivory eff ()
setOutputOptions = call_ gpio_output_options_set

gpio_output_options_set :: Def ('[GPIO_PERIPH, GPIO_OTYPE, GPIO_SPEED, GPIO_PIN] :-> ())
gpio_output_options_set = fun "gpio_output_options_set"

setBit :: GPIO_PERIPH -> GPIO_PIN -> Ivory eff ()
setBit = call_ gpio_bit_set

gpio_bit_set :: Def ('[GPIO_PERIPH, GPIO_PIN] :-> ())
gpio_bit_set = fun "gpio_bit_set"

resetBit :: GPIO_PERIPH -> GPIO_PIN -> Ivory eff ()
resetBit = call_ gpio_bit_reset

gpio_bit_reset :: Def ('[GPIO_PERIPH, GPIO_PIN] :-> ())
gpio_bit_reset = fun "gpio_bit_reset"

toggleBit :: GPIO_PERIPH -> GPIO_PIN -> Ivory eff ()
toggleBit = call_ gpio_bit_toggle

gpio_bit_toggle :: Def ('[GPIO_PERIPH, GPIO_PIN] :-> ())
gpio_bit_toggle = fun "gpio_bit_toggle"

setAF :: GPIO_PERIPH -> GPIO_AF -> GPIO_PIN -> Ivory eff ()
setAF = call_ gpio_af_set

gpio_af_set :: Def ('[GPIO_PERIPH, GPIO_AF, GPIO_PIN] :-> ())
gpio_af_set = fun "gpio_af_set"

getInputBit :: GPIO_PERIPH -> GPIO_PIN -> Ivory eff IBool
getInputBit = call gpio_input_bit_get

gpio_input_bit_get :: Def ('[GPIO_PERIPH, GPIO_PIN] :-> IBool)
gpio_input_bit_get = fun "gpio_input_bit_get"

getOutputBit :: GPIO_PERIPH -> GPIO_PIN -> Ivory eff IBool
getOutputBit = call gpio_output_bit_get

gpio_output_bit_get :: Def ('[GPIO_PERIPH, GPIO_PIN] :-> IBool)
gpio_output_bit_get = fun "gpio_output_bit_get"

inclGPIO :: ModuleDef
inclGPIO = do
    inclSym gpioa
    inclSym gpiob

    inclSym gpio_mode_input
    inclSym gpio_mode_output
    inclSym gpio_mode_af
    inclSym gpio_mode_analog

    inclSym gpio_pupd_none
    inclSym gpio_pupd_pullup
    inclSym gpio_pupd_pulldown

    inclSym gpio_ospeed_50mhz

    inclSym gpio_otype_pp
    inclSym gpio_otype_od

    inclSym gpio_pin_0
    inclSym gpio_pin_1
    inclSym gpio_pin_2
    inclSym gpio_pin_3
    inclSym gpio_pin_4
    inclSym gpio_pin_5
    inclSym gpio_pin_6
    inclSym gpio_pin_7
    inclSym gpio_pin_8
    inclSym gpio_pin_9
    inclSym gpio_pin_10
    inclSym gpio_pin_11
    inclSym gpio_pin_12
    inclSym gpio_pin_13
    inclSym gpio_pin_14
    inclSym gpio_pin_15

    inclSym gpio_af_0
    inclSym gpio_af_1
    inclSym gpio_af_2
    inclSym gpio_af_4
    inclSym gpio_af_5

    incl gpio_output_options_set
    incl gpio_mode_set
    incl gpio_af_set
    incl gpio_bit_reset
    incl gpio_bit_set
    incl gpio_bit_toggle
    incl gpio_input_bit_get
    incl gpio_output_bit_get
