{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.Misc (
    NVIC_VECTTAB,
    nvic_vecttab_flash,
    enableIrqNvic,
    setVectorTableNvic,
    inclMisc,
) where

import Ivory.Language
import Ivory.Support.Device.GD32F3x0
import Support.Device.GD32F3x0.IRQ

newtype NVIC_VECTTAB = NVIC_VECTTAB Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

nvic_vecttab_flash = NVIC_VECTTAB $ ext "NVIC_VECTTAB_FLASH"


enableIrqNvic :: IRQn -> Uint8 -> Uint8 -> Ivory eff ()
enableIrqNvic = call_ nvic_irq_enable

nvic_irq_enable :: Def ('[IRQn, Uint8, Uint8] :-> ())
nvic_irq_enable = fun "nvic_irq_enable"


setVectorTableNvic :: NVIC_VECTTAB -> Uint32 -> Ivory eff ()
setVectorTableNvic = call_ nvic_vector_table_set

nvic_vector_table_set :: Def ('[NVIC_VECTTAB, Uint32] :-> ())
nvic_vector_table_set = fun "nvic_vector_table_set"


inclMisc :: ModuleDef
inclMisc = do 
    inclSym nvic_vecttab_flash

    incl nvic_irq_enable
    incl nvic_vector_table_set
