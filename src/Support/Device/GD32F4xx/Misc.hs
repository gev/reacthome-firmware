{-# LANGUAGE DataKinds     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F4xx.Misc
    ( NVIC_VECTTAB
    , nvic_vecttab_flash

    , PRIORITY_GROUP
    , nvic_prigroup_pre2_sub2

    , SYSTICK_CLOCK_SOURCE
    , systick_clksource_hclk
    
    , enableIrqNvic
    , setVectorTableNvic
    , setPriorityGroup
    , setSystickClockSource

    , inclMisc
    ) where

import           Ivory.Language
import           Ivory.Support.Device.GD32F4xx
import           Support.Device.GD32F4xx.IRQ


newtype NVIC_VECTTAB = NVIC_VECTTAB Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

nvic_vecttab_flash  = NVIC_VECTTAB $ ext "NVIC_VECTTAB_FLASH"


newtype PRIORITY_GROUP = PRIORITY_GROUP Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

nvic_prigroup_pre2_sub2 = PRIORITY_GROUP $ ext "NVIC_PRIGROUP_PRE2_SUB2"


newtype SYSTICK_CLOCK_SOURCE = SYSTICK_CLOCK_SOURCE Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

systick_clksource_hclk = SYSTICK_CLOCK_SOURCE $ ext "SYSTICK_CLKSOURCE_HCLK"



enableIrqNvic :: IRQn -> Uint8 -> Uint8 -> Ivory eff ()
enableIrqNvic = call_ nvic_irq_enable

nvic_irq_enable :: Def ('[IRQn, Uint8, Uint8] :-> ())
nvic_irq_enable = fun "nvic_irq_enable"


setVectorTableNvic :: NVIC_VECTTAB -> Uint32 -> Ivory eff ()
setVectorTableNvic = call_ nvic_vector_table_set

nvic_vector_table_set :: Def ('[NVIC_VECTTAB, Uint32] :-> ())
nvic_vector_table_set = fun "nvic_vector_table_set"


setPriorityGroup :: PRIORITY_GROUP -> Ivory eff ()
setPriorityGroup = call_ nvic_priority_group_set

nvic_priority_group_set :: Def ('[PRIORITY_GROUP] :-> ())
nvic_priority_group_set = fun "nvic_priority_group_set"


setSystickClockSource :: SYSTICK_CLOCK_SOURCE -> Ivory eff ()
setSystickClockSource = call_ systick_clksource_set

systick_clksource_set :: Def ('[SYSTICK_CLOCK_SOURCE] :-> ())
systick_clksource_set = fun "systick_clksource_set"


inclMisc :: ModuleDef
inclMisc = do   
    inclSym nvic_vecttab_flash
    
    inclSym nvic_prigroup_pre2_sub2
    
    inclSym systick_clksource_hclk

    incl nvic_irq_enable
    incl nvic_vector_table_set
    incl nvic_priority_group_set
    incl systick_clksource_set
