{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F4xx.DMA (
    DMA_SINGLE_PARAM_STRUCT,
    DMA_SINGLE_PARAM,
    periph_addr,
    periph_inc,
    memory0_addr,
    memory_inc,
    periph_memory_width,
    circular_mode,
    direction,
    number,
    priority,
    dmaParam,
    DMA_PERIPH,
    dma0,
    dma1,
    DMA_CHANNEL,
    dma_ch0,
    dma_ch1,
    dma_ch2,
    dma_ch3,
    dma_ch4,
    dma_ch5,
    dma_ch6,
    dma_ch7,
    DMA_MEMORY_INC,
    dma_memory_increase_disable,
    dma_memory_increase_enable,
    DMA_MEMORY_WIDTH,
    dma_memory_width_8bit,
    dma_memory_width_16bit,
    dma_memory_width_32bit,
    DMA_PERIPH_INC,
    dma_periph_increase_disable,
    dma_periph_increase_enable,
    dma_periph_increase_fix,
    DMA_PERIPH_WIDTH,
    dma_periph_width_8bit,
    dma_periph_width_16bit,
    dma_periph_width_32bit,
    DMA_PRIORITY,
    dma_priority_low,
    dma_priority_medium,
    dma_priority_high,
    dma_priority_ultra_high,
    DMA_INT,
    dma_chxctl_ftfie,
    DMA_INT_FLAG,
    dma_int_flag_ftf,
    DMA_CIRCULAR_MODE,
    dma_circular_mode_enable,
    dma_circular_mode_disable,
    DMA_SUBPERIPH,
    dma_subperi0,
    dma_subperi1,
    dma_subperi2,
    dma_subperi3,
    dma_subperi4,
    dma_subperi5,
    dma_subperi6,
    dma_subperi7,
    DMA_DIRECTION,
    dma_periph_to_memory,
    dma_memory_to_periph,
    dma_memory_to_memory,
    deinitDMA,
    initSingleDMA,
    disableCirculationDMA,
    selectChannelSubperipheralDMA,
    enableChannelDMA,
    enableInterruptDMA,
    getInterruptFlagDMA,
    clearInterruptFlagDMA,
    inclDMA,
) where

import Ivory.Language
import Ivory.Support
import Ivory.Support.Device.GD32F4xx

type DMA_SINGLE_PARAM_STRUCT = "dma_single_data_parameter_struct"
type DMA_SINGLE_PARAM s = Ref s (Struct DMA_SINGLE_PARAM_STRUCT)

[ivory|
    struct dma_single_data_parameter_struct
        { periph_addr :: Stored Uint32
        ; periph_inc :: Stored DMA_PERIPH_INC
        ; memory0_addr :: Stored Uint32
        ; memory_inc :: Stored DMA_MEMORY_INC
        ; periph_memory_width :: Stored DMA_PERIPH_WIDTH
        ; circular_mode :: Stored DMA_CIRCULAR_MODE
        ; direction :: Stored DMA_DIRECTION
        ; number :: Stored Uint32
        ; priority :: Stored DMA_PRIORITY
        }
|]

dmaParam ::
    [InitStruct DMA_SINGLE_PARAM_STRUCT] ->
    [InitStruct DMA_SINGLE_PARAM_STRUCT]
dmaParam p =
    p
        <+> [ periph_addr .= ival 0
            , periph_inc .= ival dma_periph_increase_disable
            , memory0_addr .= ival 0
            , memory_inc .= ival dma_memory_increase_disable
            , periph_memory_width .= ival dma_periph_width_8bit
            , circular_mode .= ival dma_circular_mode_disable
            , direction .= ival dma_periph_to_memory
            , number .= ival 0
            , priority .= ival dma_priority_low
            ]

newtype DMA_PERIPH = DMA_PERIPH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
instance ExtSymbol DMA_PERIPH

dma0 = DMA_PERIPH $ ext "DMA0"
dma1 = DMA_PERIPH $ ext "DMA1"

newtype DMA_CHANNEL = DMA_CHANNEL Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
instance ExtSymbol DMA_CHANNEL

dma_ch0 = DMA_CHANNEL $ ext "DMA_CH0"
dma_ch1 = DMA_CHANNEL $ ext "DMA_CH1"
dma_ch2 = DMA_CHANNEL $ ext "DMA_CH2"
dma_ch3 = DMA_CHANNEL $ ext "DMA_CH3"
dma_ch4 = DMA_CHANNEL $ ext "DMA_CH4"
dma_ch5 = DMA_CHANNEL $ ext "DMA_CH5"
dma_ch6 = DMA_CHANNEL $ ext "DMA_CH6"
dma_ch7 = DMA_CHANNEL $ ext "DMA_CH7"

newtype DMA_DIRECTION = DMA_DIRECTION Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_periph_to_memory = DMA_DIRECTION $ ext "DMA_PERIPH_TO_MEMORY"
dma_memory_to_periph = DMA_DIRECTION $ ext "DMA_MEMORY_TO_PERIPH"
dma_memory_to_memory = DMA_DIRECTION $ ext "DMA_MEMORY_TO_MEMORY"

newtype DMA_MEMORY_INC = DMA_MEMORY_INC Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_memory_increase_disable = DMA_MEMORY_INC $ ext "DMA_MEMORY_INCREASE_DISABLE"
dma_memory_increase_enable = DMA_MEMORY_INC $ ext "DMA_MEMORY_INCREASE_ENABLE"

newtype DMA_MEMORY_WIDTH = DMA_MEMORY_WIDTH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_memory_width_8bit = DMA_MEMORY_WIDTH $ ext "DMA_MEMORY_WIDTH_8BIT"
dma_memory_width_16bit = DMA_MEMORY_WIDTH $ ext "DMA_MEMORY_WIDTH_16BIT"
dma_memory_width_32bit = DMA_MEMORY_WIDTH $ ext "DMA_MEMORY_WIDTH_32BIT"

newtype DMA_PERIPH_INC = DMA_PERIPH_INC Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_periph_increase_disable = DMA_PERIPH_INC $ ext "DMA_PERIPH_INCREASE_DISABLE"
dma_periph_increase_enable = DMA_PERIPH_INC $ ext "DMA_PERIPH_INCREASE_ENABLE"
dma_periph_increase_fix = DMA_PERIPH_INC $ ext "DMA_PERIPH_INCREASE_FIX"

newtype DMA_PERIPH_WIDTH = DMA_PERIPH_WIDTH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_periph_width_8bit = DMA_PERIPH_WIDTH $ ext "DMA_PERIPH_WIDTH_8BIT"
dma_periph_width_16bit = DMA_PERIPH_WIDTH $ ext "DMA_PERIPH_WIDTH_16BIT"
dma_periph_width_32bit = DMA_PERIPH_WIDTH $ ext "DMA_PERIPH_WIDTH_32BIT"

newtype DMA_PRIORITY = DMA_PRIORITY Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_priority_low = DMA_PRIORITY $ ext "DMA_PRIORITY_LOW"
dma_priority_medium = DMA_PRIORITY $ ext "DMA_PRIORITY_MEDIUM"
dma_priority_high = DMA_PRIORITY $ ext "DMA_PRIORITY_HIGH"
dma_priority_ultra_high = DMA_PRIORITY $ ext "DMA_PRIORITY_ULTRA_HIGH"

newtype DMA_INT_FLAG = DMA_INT_FLAG Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_int_flag_ftf = DMA_INT_FLAG $ ext "DMA_INT_FLAG_FTF"

newtype DMA_INT = DMA_INT Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_chxctl_ftfie = DMA_INT $ ext "DMA_CHXCTL_FTFIE"

newtype DMA_CIRCULAR_MODE = DMA_CIRCULAR_MODE Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_circular_mode_enable = DMA_CIRCULAR_MODE $ ext "DMA_CIRCULAR_MODE_ENABLE"
dma_circular_mode_disable = DMA_CIRCULAR_MODE $ ext "DMA_CIRCULAR_MODE_DISABLE"

newtype DMA_SUBPERIPH = DMA_SUBPERIPH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_subperi0 = DMA_SUBPERIPH $ ext "DMA_SUBPERI0"
dma_subperi1 = DMA_SUBPERIPH $ ext "DMA_SUBPERI1"
dma_subperi2 = DMA_SUBPERIPH $ ext "DMA_SUBPERI2"
dma_subperi3 = DMA_SUBPERIPH $ ext "DMA_SUBPERI3"
dma_subperi4 = DMA_SUBPERIPH $ ext "DMA_SUBPERI4"
dma_subperi5 = DMA_SUBPERIPH $ ext "DMA_SUBPERI5"
dma_subperi6 = DMA_SUBPERIPH $ ext "DMA_SUBPERI6"
dma_subperi7 = DMA_SUBPERIPH $ ext "DMA_SUBPERI7"

deinitDMA :: DMA_PERIPH -> DMA_CHANNEL -> Ivory eff ()
deinitDMA = call_ dma_deinit

dma_deinit :: Def ('[DMA_PERIPH, DMA_CHANNEL] :-> ())
dma_deinit = fun "dma_deinit"

initSingleDMA :: DMA_PERIPH -> DMA_CHANNEL -> DMA_SINGLE_PARAM s -> Ivory eff ()
initSingleDMA = call_ dma_single_data_mode_init

dma_single_data_mode_init :: Def ('[DMA_PERIPH, DMA_CHANNEL, DMA_SINGLE_PARAM s] :-> ())
dma_single_data_mode_init = fun "dma_single_data_mode_init"

disableCirculationDMA :: DMA_PERIPH -> DMA_CHANNEL -> Ivory eff ()
disableCirculationDMA = call_ dma_circulation_disable

dma_circulation_disable :: Def ('[DMA_PERIPH, DMA_CHANNEL] :-> ())
dma_circulation_disable = fun "dma_circulation_disable"

selectChannelSubperipheralDMA :: DMA_PERIPH -> DMA_CHANNEL -> DMA_SUBPERIPH -> Ivory eff ()
selectChannelSubperipheralDMA = call_ dma_channel_subperipheral_select

dma_channel_subperipheral_select :: Def ('[DMA_PERIPH, DMA_CHANNEL, DMA_SUBPERIPH] :-> ())
dma_channel_subperipheral_select = fun "dma_channel_subperipheral_select"

enableChannelDMA :: DMA_PERIPH -> DMA_CHANNEL -> Ivory eff ()
enableChannelDMA = call_ dma_channel_enable

dma_channel_enable :: Def ('[DMA_PERIPH, DMA_CHANNEL] :-> ())
dma_channel_enable = fun "dma_channel_enable"

enableInterruptDMA :: DMA_PERIPH -> DMA_CHANNEL -> DMA_INT -> Ivory eff ()
enableInterruptDMA = call_ dma_interrupt_enable

dma_interrupt_enable :: Def ('[DMA_PERIPH, DMA_CHANNEL, DMA_INT] :-> ())
dma_interrupt_enable = fun "dma_interrupt_enable"

getInterruptFlagDMA :: DMA_PERIPH -> DMA_CHANNEL -> DMA_INT_FLAG -> Ivory eff IBool
getInterruptFlagDMA = call dma_interrupt_flag_get

dma_interrupt_flag_get :: Def ('[DMA_PERIPH, DMA_CHANNEL, DMA_INT_FLAG] :-> IBool)
dma_interrupt_flag_get = fun "dma_interrupt_flag_get"

clearInterruptFlagDMA :: DMA_PERIPH -> DMA_CHANNEL -> DMA_INT_FLAG -> Ivory eff ()
clearInterruptFlagDMA = call_ dma_interrupt_flag_clear

dma_interrupt_flag_clear :: Def ('[DMA_PERIPH, DMA_CHANNEL, DMA_INT_FLAG] :-> ())
dma_interrupt_flag_clear = fun "dma_interrupt_flag_clear"

inclDMA :: ModuleDef
inclDMA = do
    inclSym dma0
    inclSym dma1

    inclSym dma_ch0
    inclSym dma_ch1
    inclSym dma_ch2
    inclSym dma_ch3
    inclSym dma_ch4
    inclSym dma_ch5
    inclSym dma_ch6
    inclSym dma_ch7

    inclSym dma_periph_to_memory
    inclSym dma_memory_to_periph
    inclSym dma_memory_to_memory

    inclSym dma_memory_increase_disable
    inclSym dma_memory_increase_enable

    inclSym dma_memory_width_8bit
    inclSym dma_memory_width_16bit
    inclSym dma_memory_width_32bit

    inclSym dma_periph_increase_disable
    inclSym dma_periph_increase_enable
    inclSym dma_periph_increase_fix

    inclSym dma_periph_width_8bit
    inclSym dma_periph_width_16bit
    inclSym dma_periph_width_32bit

    inclSym dma_priority_low
    inclSym dma_priority_medium
    inclSym dma_priority_high
    inclSym dma_priority_ultra_high

    inclSym dma_int_flag_ftf

    inclSym dma_chxctl_ftfie

    inclSym dma_circular_mode_enable
    inclSym dma_circular_mode_disable

    inclSym dma_subperi0
    inclSym dma_subperi1
    inclSym dma_subperi2
    inclSym dma_subperi3
    inclSym dma_subperi4
    inclSym dma_subperi5
    inclSym dma_subperi6
    inclSym dma_subperi7

    incl dma_deinit
    incl dma_single_data_mode_init
    incl dma_circulation_disable
    incl dma_channel_subperipheral_select
    incl dma_channel_enable
    incl dma_interrupt_enable
    incl dma_interrupt_flag_get
    incl dma_interrupt_flag_clear
