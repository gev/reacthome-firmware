{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.DMA (
    DMA_PARAM_STRUCT,
    DMA_PARAM,
    periph_addr,
    periph_width,
    periph_inc,
    memory_addr,
    memory_width,
    memory_inc,
    direction,
    number,
    priority,
    dmaParam,
    DMA_CHANNEL,
    dma_ch0,
    dma_ch1,
    dma_ch2,
    dma_ch3,
    dma_ch4,
    dma_ch5,
    dma_ch6,
    DMA_DIRECTION,
    dma_peripheral_to_memory,
    dma_memory_to_peripheral,
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
    DMA_PERIPH_WIDTH,
    dma_peripheral_width_8bit,
    dma_peripheral_width_16bit,
    dma_peripheral_width_32bit,
    DMA_PRIORITY,
    dma_priority_low,
    dma_priority_medium,
    dma_priority_high,
    dma_priority_ultra_high,
    DMA_INT,
    dma_int_flag_ftf,
    dma_int_flag_g,
    DMA_INT_FLAG,
    dma_int_ftf,
    deinitDMA,
    initDMA,
    disableCirculationDMA,
    enableCirculationDMA,
    disableMemoryToMemoryDMA,
    enableChannelDMA,
    enableInterruptDMA,
    getInterruptFlagDMA,
    clearInterruptFlagDMA,
    inclDMA,
) where

import Ivory.Language
import Ivory.Support
import Ivory.Support.Device.GD32F3x0

type DMA_PARAM_STRUCT = "dma_parameter_struct"
type DMA_PARAM s = Ref s (Struct DMA_PARAM_STRUCT)

[ivory|
    struct dma_parameter_struct
        { periph_addr :: Stored Uint32
        ; periph_width :: Stored DMA_PERIPH_WIDTH
        ; periph_inc :: Stored DMA_PERIPH_INC
        ; memory_addr :: Stored Uint32
        ; memory_width :: Stored DMA_MEMORY_WIDTH
        ; memory_inc :: Stored DMA_MEMORY_INC
        ; direction :: Stored DMA_DIRECTION
        ; number :: Stored Uint16
        ; priority :: Stored DMA_PRIORITY
        }
|]

dmaParam ::
    [InitStruct DMA_PARAM_STRUCT] ->
    [InitStruct DMA_PARAM_STRUCT]
dmaParam p =
    p
        <+> [ periph_addr .= ival 0
            , periph_width .= ival dma_peripheral_width_8bit
            , periph_inc .= ival dma_periph_increase_disable
            , memory_addr .= ival 0
            , memory_width .= ival dma_memory_width_8bit
            , memory_inc .= ival dma_memory_increase_disable
            , direction .= ival dma_peripheral_to_memory
            , number .= ival 0
            , priority .= ival dma_priority_low
            ]

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

newtype DMA_DIRECTION = DMA_DIRECTION Uint8
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_peripheral_to_memory = DMA_DIRECTION $ ext "DMA_PERIPHERAL_TO_MEMORY"
dma_memory_to_peripheral = DMA_DIRECTION $ ext "DMA_MEMORY_TO_PERIPHERAL"

newtype DMA_MEMORY_INC = DMA_MEMORY_INC Uint8
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_memory_increase_disable = DMA_MEMORY_INC $ ext "DMA_MEMORY_INCREASE_DISABLE"
dma_memory_increase_enable = DMA_MEMORY_INC $ ext "DMA_MEMORY_INCREASE_ENABLE"

newtype DMA_MEMORY_WIDTH = DMA_MEMORY_WIDTH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_memory_width_8bit = DMA_MEMORY_WIDTH $ ext "DMA_MEMORY_WIDTH_8BIT"
dma_memory_width_16bit = DMA_MEMORY_WIDTH $ ext "DMA_MEMORY_WIDTH_16BIT"
dma_memory_width_32bit = DMA_MEMORY_WIDTH $ ext "DMA_MEMORY_WIDTH_32BIT"

newtype DMA_PERIPH_INC = DMA_PERIPH_INC Uint8
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_periph_increase_disable = DMA_PERIPH_INC $ ext "DMA_PERIPH_INCREASE_DISABLE"
dma_periph_increase_enable = DMA_PERIPH_INC $ ext "DMA_PERIPH_INCREASE_ENABLE"

newtype DMA_PERIPH_WIDTH = DMA_PERIPH_WIDTH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_peripheral_width_8bit = DMA_PERIPH_WIDTH $ ext "DMA_PERIPHERAL_WIDTH_8BIT"
dma_peripheral_width_16bit = DMA_PERIPH_WIDTH $ ext "DMA_PERIPHERAL_WIDTH_16BIT"
dma_peripheral_width_32bit = DMA_PERIPH_WIDTH $ ext "DMA_PERIPHERAL_WIDTH_32BIT"

newtype DMA_PRIORITY = DMA_PRIORITY Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_priority_low = DMA_PRIORITY $ ext "DMA_PRIORITY_LOW"
dma_priority_medium = DMA_PRIORITY $ ext "DMA_PRIORITY_MEDIUM"
dma_priority_high = DMA_PRIORITY $ ext "DMA_PRIORITY_HIGH"
dma_priority_ultra_high = DMA_PRIORITY $ ext "DMA_PRIORITY_ULTRA_HIGH"

newtype DMA_INT_FLAG = DMA_INT_FLAG Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_int_flag_ftf = DMA_INT_FLAG $ ext "DMA_INT_FLAG_FTF"
dma_int_flag_g = DMA_INT_FLAG $ ext "DMA_INT_FLAG_G"

newtype DMA_INT = DMA_INT Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dma_int_ftf = DMA_INT $ ext "DMA_INT_FTF"

deinitDMA :: DMA_CHANNEL -> Ivory eff ()
deinitDMA = call_ dma_deinit

dma_deinit :: Def ('[DMA_CHANNEL] :-> ())
dma_deinit = fun "dma_deinit"

initDMA :: DMA_CHANNEL -> DMA_PARAM s -> Ivory eff ()
initDMA = call_ dma_init

dma_init :: Def ('[DMA_CHANNEL, DMA_PARAM s] :-> ())
dma_init = fun "dma_init"

disableCirculationDMA :: DMA_CHANNEL -> Ivory eff ()
disableCirculationDMA = call_ dma_circulation_disable

dma_circulation_disable :: Def ('[DMA_CHANNEL] :-> ())
dma_circulation_disable = fun "dma_circulation_disable"

enableCirculationDMA :: DMA_CHANNEL -> Ivory eff ()
enableCirculationDMA = call_ dma_circulation_enable

dma_circulation_enable :: Def ('[DMA_CHANNEL] :-> ())
dma_circulation_enable = fun "dma_circulation_enable"

disableMemoryToMemoryDMA :: DMA_CHANNEL -> Ivory eff ()
disableMemoryToMemoryDMA = call_ dma_memory_to_memory_disable

dma_memory_to_memory_disable :: Def ('[DMA_CHANNEL] :-> ())
dma_memory_to_memory_disable = fun "dma_memory_to_memory_disable"

enableChannelDMA :: DMA_CHANNEL -> Ivory eff ()
enableChannelDMA = call_ dma_channel_enable

dma_channel_enable :: Def ('[DMA_CHANNEL] :-> ())
dma_channel_enable = fun "dma_channel_enable"

enableInterruptDMA :: DMA_CHANNEL -> DMA_INT -> Ivory eff ()
enableInterruptDMA = call_ dma_interrupt_enable

dma_interrupt_enable :: Def ('[DMA_CHANNEL, DMA_INT] :-> ())
dma_interrupt_enable = fun "dma_interrupt_enable"

getInterruptFlagDMA :: DMA_CHANNEL -> DMA_INT_FLAG -> Ivory eff IBool
getInterruptFlagDMA = call dma_interrupt_flag_get

dma_interrupt_flag_get :: Def ('[DMA_CHANNEL, DMA_INT_FLAG] :-> IBool)
dma_interrupt_flag_get = fun "dma_interrupt_flag_get"

clearInterruptFlagDMA :: DMA_CHANNEL -> DMA_INT_FLAG -> Ivory eff ()
clearInterruptFlagDMA = call_ dma_interrupt_flag_clear

dma_interrupt_flag_clear :: Def ('[DMA_CHANNEL, DMA_INT_FLAG] :-> ())
dma_interrupt_flag_clear = fun "dma_interrupt_flag_clear"

inclDMA :: ModuleDef
inclDMA = do
    inclSym dma_ch0
    inclSym dma_ch1
    inclSym dma_ch2
    inclSym dma_ch3
    inclSym dma_ch4
    inclSym dma_ch5
    inclSym dma_ch6

    inclSym dma_peripheral_to_memory
    inclSym dma_memory_to_peripheral

    inclSym dma_memory_increase_disable
    inclSym dma_memory_increase_enable

    inclSym dma_memory_width_8bit
    inclSym dma_memory_width_16bit
    inclSym dma_memory_width_32bit

    inclSym dma_periph_increase_disable
    inclSym dma_periph_increase_enable

    inclSym dma_peripheral_width_8bit
    inclSym dma_peripheral_width_16bit
    inclSym dma_peripheral_width_32bit

    inclSym dma_priority_low
    inclSym dma_priority_medium
    inclSym dma_priority_high
    inclSym dma_priority_ultra_high

    inclSym dma_int_flag_ftf
    inclSym dma_int_flag_g

    inclSym dma_int_ftf

    incl dma_deinit
    incl dma_init
    incl dma_circulation_disable
    incl dma_circulation_enable
    incl dma_memory_to_memory_disable
    incl dma_channel_enable
    incl dma_interrupt_enable
    incl dma_interrupt_flag_get
    incl dma_interrupt_flag_clear
