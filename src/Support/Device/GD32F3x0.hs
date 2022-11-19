{-# LANGUAGE MultiParamTypeClasses #-}
module Support.Device.GD32F3x0
  ( IRQn (..)
  , inclG
  ) where

import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Language.Proc
import           Ivory.Language.Syntax
import           Support.Ivory

(cast, _) = include "gd32f3x0.h"


data IRQn
  = TIMER2_IRQn
  deriving (Show, Enum, Bounded)
instance ExtCast IRQn Uint8


inclG :: ModuleM ()
inclG = do
  inclDef (cast :: Cast IRQn Uint8)
