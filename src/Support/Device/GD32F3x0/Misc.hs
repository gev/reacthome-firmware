{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.Misc
    ( enableIrqNvic
    , inclMisc
    ) where

import           Ivory.Language
import           Ivory.Support.Device.GD32F3x0
import           Support.Device.GD32F3x0.IRQ



enableIrqNvic :: IRQn -> Uint8 -> Uint8 -> Ivory eff ()
enableIrqNvic = call_ nvic_irq_enable

nvic_irq_enable :: Def ('[IRQn, Uint8, Uint8] :-> ())
nvic_irq_enable = fun "nvic_irq_enable"



inclMisc :: ModuleDef
inclMisc =    incl nvic_irq_enable
