{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Support.Device.GD32F3x0.Misc
  ( enableIrqNvic
  , inclMisc
  ) where

import           Ivory.Language
import           Ivory.Language.Module
import           Support.Device.GD32F3x0
import           Support.Ivory

(def, _) = include "gd32f3x0.h"
(_, fun) = include "gd32f3x0_misc.h"


inclMisc :: ModuleM ()
inclMisc = do
  incl nvic_irq_enable


enableIrqNvic :: IRQn -> Uint8 -> Uint8 -> Ivory eff ()
enableIrqNvic irqn = call_ nvic_irq_enable (def irqn)

nvic_irq_enable :: Def ('[Uint8, Uint8, Uint8] :-> ())
nvic_irq_enable = fun "nvic_irq_enable"
