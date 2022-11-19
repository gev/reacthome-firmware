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
  , setMode
  , setOutputOptions
  , setBit
  , resetBit
  , inclGPIO
  ) where

import           Ivory.Language        (Def, Ivory, Proc ((:->)), Uint32, call_)
import           Ivory.Language.Module
import           Support.Ivory

(cast, fun) = include "gd32f3x0_gpio.h"


data GPIO_PERIPH
  = GPIOA
  deriving (Show, Enum, Bounded)
instance ExtCast GPIO_PERIPH Uint32

data GPIO_MODE
  = GPIO_MODE_INPUT
  | GPIO_MODE_OUTPUT
  deriving (Show, Enum, Bounded)
instance ExtCast GPIO_MODE Uint32

data GPIO_PUPD
  = GPIO_PUPD_NONE
  deriving (Show, Enum, Bounded)
instance ExtCast GPIO_PUPD Uint32

data GPIO_SPEED
  = GPIO_OSPEED_50MHZ
  deriving (Show, Enum, Bounded)
instance ExtCast GPIO_SPEED Uint32

data GPIO_OTYPE
  = GPIO_OTYPE_PP
  deriving (Show, Enum, Bounded)
instance ExtCast GPIO_OTYPE Uint32

data GPIO_PIN
  = GPIO_PIN_4
  | GPIO_PIN_15
  deriving (Show, Enum, Bounded)
instance ExtCast GPIO_PIN Uint32


inclGPIO :: ModuleM ()
inclGPIO = do
  inclDef (cast :: Cast GPIO_PERIPH Uint32)
  inclDef (cast :: Cast GPIO_MODE Uint32)
  inclDef (cast :: Cast GPIO_PUPD Uint32)
  inclDef (cast :: Cast GPIO_OTYPE Uint32)
  inclDef (cast :: Cast GPIO_SPEED Uint32)
  inclDef (cast :: Cast GPIO_PIN Uint32)
  incl gpio_mode_set
  incl gpio_output_options_set
  incl gpio_bit_set
  incl gpio_bit_reset


setMode :: GPIO_PERIPH -> GPIO_MODE -> GPIO_PUPD -> GPIO_PIN -> Ivory eff ()
setMode gpio mode pupd pin =
  call_ gpio_mode_set (cast gpio) (cast mode) (cast pupd) (cast pin)

gpio_mode_set :: Def ('[Uint32, Uint32, Uint32, Uint32] :-> ())
gpio_mode_set = fun "gpio_mode_set"


setOutputOptions :: GPIO_PERIPH -> GPIO_OTYPE -> GPIO_SPEED -> GPIO_PIN -> Ivory eff ()
setOutputOptions gpio otype speed pin =
  call_ gpio_output_options_set (cast gpio) (cast otype) (cast speed) (cast pin)

gpio_output_options_set :: Def ('[Uint32, Uint32, Uint32, Uint32] :-> ())
gpio_output_options_set = fun "gpio_output_options_set"


setBit :: GPIO_PERIPH -> GPIO_PIN -> Ivory eff ()
setBit gpio pin =
  call_ gpio_bit_set (cast gpio) (cast pin)

gpio_bit_set :: Def ('[Uint32, Uint32] :-> ())
gpio_bit_set = fun "gpio_bit_set"


resetBit :: GPIO_PERIPH -> GPIO_PIN -> Ivory eff ()
resetBit gpio pin =
  call_ gpio_bit_reset (cast gpio) (cast pin)

gpio_bit_reset :: Def ('[Uint32, Uint32] :-> ())
gpio_bit_reset = fun "gpio_bit_reset"
