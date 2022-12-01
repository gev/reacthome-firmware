{-# LANGUAGE MultiParamTypeClasses #-}

module Support.Device.GD32F3x0
  ( IRQn (..)
  , inclG
  ) where

import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Language.Proc
import           Ivory.Language.Syntax
import           Ivory.Support
import           Ivory.Support.Device.GD32F3x0


data IRQn
  = TIMER2_IRQn
  deriving (Show, Enum, Bounded)
instance ExtDef IRQn Uint8


inclG :: ModuleM ()
inclG = do
  inclDef (def :: Cast IRQn Uint8)
