{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.GPIO
    ( GPIO_PERIPH (..)
    , GPIO_MODE   (..)
    , GPIO_PUPD   (..)
    , GPIO_OTYPE  (..)
    , GPIO_SPEED  (..)
    , GPIO_PIN    (..)
    , GPIO_AF     (..)
    , setMode
    , setOutputOptions
    , setBit
    , resetBit
    , setAF
    , inclGPIO
    ) where

import           Ivory.Language                (Def, Ivory, Proc ((:->)),
                                                Uint32, call_)
import           Ivory.Language.Module
import           Ivory.Support
import           Ivory.Support.Device.GD32F3x0


data GPIO_PERIPH
    = GPIOA
    | GPIOB
    deriving (Show, Enum, Bounded)
instance ExtDef GPIO_PERIPH Uint32

data GPIO_MODE
    = GPIO_MODE_INPUT
    | GPIO_MODE_OUTPUT
    | GPIO_MODE_AF
    deriving (Show, Enum, Bounded)
instance ExtDef GPIO_MODE Uint32

data GPIO_PUPD
    = GPIO_PUPD_NONE
    | GPIO_PUPD_PULLUP
    | GPIO_PUPD_PULLDOWN
    deriving (Show, Enum, Bounded)
instance ExtDef GPIO_PUPD Uint32

data GPIO_SPEED
    = GPIO_OSPEED_50MHZ
    deriving (Show, Enum, Bounded)
instance ExtDef GPIO_SPEED Uint32

data GPIO_OTYPE
    = GPIO_OTYPE_PP
    deriving (Show, Enum, Bounded)
instance ExtDef GPIO_OTYPE Uint32

data GPIO_PIN
    = GPIO_PIN_2
    | GPIO_PIN_3
    | GPIO_PIN_4
    | GPIO_PIN_15
    deriving (Show, Enum, Bounded)
instance ExtDef GPIO_PIN Uint32

data GPIO_AF
    = GPIO_AF_1
    deriving (Show, Enum, Bounded)
instance ExtDef GPIO_AF Uint32

inclGPIO :: ModuleM ()
inclGPIO = do
    inclDef (def :: Cast GPIO_PERIPH Uint32)
    inclDef (def :: Cast GPIO_MODE Uint32)
    inclDef (def :: Cast GPIO_PUPD Uint32)
    inclDef (def :: Cast GPIO_OTYPE Uint32)
    inclDef (def :: Cast GPIO_SPEED Uint32)
    inclDef (def :: Cast GPIO_PIN Uint32)
    inclDef (def :: Cast GPIO_AF Uint32)
    incl gpio_mode_set
    incl gpio_output_options_set
    incl gpio_bit_set
    incl gpio_bit_reset
    incl gpio_af_set


setMode :: GPIO_PERIPH -> GPIO_MODE -> GPIO_PUPD -> GPIO_PIN -> Ivory eff ()
setMode gpio mode pupd pin =
    call_ gpio_mode_set (def gpio) (def mode) (def pupd) (def pin)

gpio_mode_set :: Def ('[Uint32, Uint32, Uint32, Uint32] :-> ())
gpio_mode_set = fun "gpio_mode_set"


setOutputOptions :: GPIO_PERIPH -> GPIO_OTYPE -> GPIO_SPEED -> GPIO_PIN -> Ivory eff ()
setOutputOptions gpio otype speed pin =
    call_ gpio_output_options_set (def gpio) (def otype) (def speed) (def pin)

gpio_output_options_set :: Def ('[Uint32, Uint32, Uint32, Uint32] :-> ())
gpio_output_options_set = fun "gpio_output_options_set"


setBit :: GPIO_PERIPH -> GPIO_PIN -> Ivory eff ()
setBit gpio pin =
    call_ gpio_bit_set (def gpio) (def pin)

gpio_bit_set :: Def ('[Uint32, Uint32] :-> ())
gpio_bit_set = fun "gpio_bit_set"


resetBit :: GPIO_PERIPH -> GPIO_PIN -> Ivory eff ()
resetBit gpio pin =
    call_ gpio_bit_reset (def gpio) (def pin)

gpio_bit_reset :: Def ('[Uint32, Uint32] :-> ())
gpio_bit_reset = fun "gpio_bit_reset"


setAF :: GPIO_PERIPH -> GPIO_AF -> GPIO_PIN -> Ivory eff ()
setAF gpio af pin =
    call_ gpio_af_set (def gpio) (def af) (def pin)

gpio_af_set :: Def ('[Uint32, Uint32, Uint32] :-> ())
gpio_af_set = fun "gpio_af_set"
