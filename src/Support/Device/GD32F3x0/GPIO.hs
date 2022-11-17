{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Support.Device.GD32F3x0.GPIO
  ( GPIO        (..)
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

import           Data.Foldable
import           Ivory.Language        (Def, Ivory, IvoryExpr, Proc ((:->)),
                                        Uint32, call_, extern, importProc, incl,
                                        inclSym)
import           Ivory.Language.Module
import           Ivory.Language.Proc
import           Ivory.Language.Syntax

data GPIO
  = GPIOA
  deriving (Show, Enum, Bounded)

data GPIO_MODE
  = GPIO_MODE_INPUT
  | GPIO_MODE_OUTPUT
  deriving (Show, Enum, Bounded)

data GPIO_PUPD
  = GPIO_PUPD_NONE
  deriving (Show, Enum, Bounded)

data GPIO_SPEED
  = GPIO_OSPEED_50MHZ
  deriving (Show, Enum, Bounded)

data GPIO_OTYPE
  = GPIO_OTYPE_PP
  deriving (Show, Enum, Bounded)

data GPIO_PIN
  = GPIO_PIN_15
  deriving (Show, Enum, Bounded)

setMode :: GPIO -> GPIO_MODE -> GPIO_PUPD -> GPIO_PIN -> Ivory eff ()
setMode gpio mode pupd pin =
  call_ gpioModeSet (extConst gpio) (extConst mode) (extConst pupd) (extConst pin)

setOutputOptions :: GPIO -> GPIO_OTYPE -> GPIO_SPEED -> GPIO_PIN -> Ivory eff ()
setOutputOptions gpio otype speed pin =
  call_ gpioOutputOptionsSet (extConst gpio) (extConst otype) (extConst speed) (extConst pin)

setBit :: GPIO -> GPIO_PIN -> Ivory eff ()
setBit gpio pin =
  call_ gpioBitSet (extConst gpio) (extConst pin)

resetBit :: GPIO -> GPIO_PIN -> Ivory eff ()
resetBit gpio pin =
  call_ gpioBitReset (extConst gpio) (extConst pin)

inclGPIO :: ModuleM ()
inclGPIO = do
  traverse_ (inclSym . extGPIO) [minBound .. maxBound]
  traverse_ (inclSym . extGPIO_MODE) [minBound .. maxBound]
  traverse_ (inclSym . extGPIO_PUPD) [minBound .. maxBound]
  traverse_ (inclSym . extGPIO_OTYPE) [minBound .. maxBound]
  traverse_ (inclSym . extGPIO_SPEED) [minBound .. maxBound]
  traverse_ (inclSym . extGPIO_PIN) [minBound .. maxBound]
  incl gpioModeSet
  incl gpioOutputOptionsSet
  incl gpioBitReset
  incl gpioBitSet

gpioModeSet :: Def ('[Uint32, Uint32, Uint32, Uint32] :-> ())
gpioModeSet = extProc "gpio_mode_set"

gpioOutputOptionsSet :: Def ('[Uint32, Uint32, Uint32, Uint32] :-> ())
gpioOutputOptionsSet = extProc "gpio_output_options_set"

gpioBitSet :: Def ('[Uint32, Uint32] :-> ())
gpioBitSet = extProc "gpio_bit_set"

gpioBitReset :: Def ('[Uint32, Uint32] :-> ())
gpioBitReset = extProc "gpio_bit_reset"

extGPIO :: GPIO -> Uint32
extGPIO = extConst

extGPIO_MODE :: GPIO_MODE -> Uint32
extGPIO_MODE = extConst

extGPIO_PUPD :: GPIO_PUPD -> Uint32
extGPIO_PUPD = extConst

extGPIO_OTYPE :: GPIO_OTYPE -> Uint32
extGPIO_OTYPE = extConst

extGPIO_SPEED :: GPIO_SPEED -> Uint32
extGPIO_SPEED = extConst

extGPIO_PIN :: GPIO_PIN -> Uint32
extGPIO_PIN = extConst

extConst :: (Show a, IvoryExpr e) => a -> e
extConst = (`extern` headerFile) . show

extProc :: ProcType t => Sym -> Def t
extProc = (`importProc` headerFile)

headerFile :: String
headerFile = "gd32f3x0_gpio.h"

