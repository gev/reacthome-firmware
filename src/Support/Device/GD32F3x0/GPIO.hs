{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

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

import           Data.Foldable
import           Ivory.Language        (Def, Ivory, IvoryExpr, Proc ((:->)),
                                        Uint32, call_, extern, importProc, incl,
                                        inclSym)
import           Ivory.Language.Module
import           Ivory.Language.Proc
import           Ivory.Language.Syntax
import           Ivory.Language.Uint   (Uint32 (Uint32))
import           Support.Ivory

data GPIO_PERIPH
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

instance ExtConst GPIO_PERIPH Uint32
instance ExtConst GPIO_MODE Uint32
instance ExtConst GPIO_OTYPE Uint32
instance ExtConst GPIO_PUPD Uint32
instance ExtConst GPIO_SPEED Uint32
instance ExtConst GPIO_PIN Uint32

setMode :: GPIO_PERIPH -> GPIO_MODE -> GPIO_PUPD -> GPIO_PIN -> Ivory eff ()
setMode gpio mode pupd pin =
  call_ gpioModeSet (extConst gpio) (extConst mode) (extConst pupd) (extConst pin)

setOutputOptions :: GPIO_PERIPH -> GPIO_OTYPE -> GPIO_SPEED -> GPIO_PIN -> Ivory eff ()
setOutputOptions gpio otype speed pin =
  call_ gpioOutputOptionsSet (extConst gpio) (extConst otype) (extConst speed) (extConst pin)

setBit :: GPIO_PERIPH -> GPIO_PIN -> Ivory eff ()
setBit gpio pin =
  call_ gpioBitSet (extConst gpio) (extConst pin)

resetBit :: GPIO_PERIPH -> GPIO_PIN -> Ivory eff ()
resetBit gpio pin =
  call_ gpioBitReset (extConst gpio) (extConst pin)

inclGPIO :: ModuleM ()
inclGPIO = do
  inclConst (extConst :: Ext GPIO_PERIPH Uint32)
  inclConst (extConst :: Ext GPIO_MODE Uint32)
  inclConst (extConst :: Ext GPIO_PUPD Uint32)
  inclConst (extConst :: Ext GPIO_OTYPE Uint32)
  inclConst (extConst :: Ext GPIO_SPEED Uint32)
  inclConst (extConst :: Ext GPIO_PIN Uint32)
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

extConst :: ExtConst a e => a -> e
extConst = extConstFrom hFile

extProc :: ProcType t => Sym -> Def t
extProc = extProcFrom hFile

hFile :: HeaderFile
hFile = "gd32f3x0_gpio.h"
