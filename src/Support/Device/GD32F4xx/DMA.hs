{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F4xx.DMA
    ( DMA_CHANNEL_IRQ   (..)
    , DMA_CHANNEL       (..)
    , DMA_PERIPH        (..)
    , DMA_DIRECTION     (..)
    , DMA_MEMORY_INC    (..)
    , DMA_MEMORY_WIDTH  (..)
    , DMA_PERIPH_INC    (..)
    , DMA_PERIPH_WIDTH  (..)
    , DMA_PRIORITY      (..)
    , DMA_SINGLE_PARAM  (..)
    , DMA_INT           (..)
    , DMA_INT_FLAG      (..)
    , DMA_CIRCULAR_MODE (..)
    , deinitDMA
    , initSingleDMA
    , enableChannelDMA
    , enableInterruptDMA
    , getInterruptFlagDMA
    , clearInterruptFlagDMA
    , dmaParam
    , inclDMA
    ) where

import           Data.Function                 ((&))
import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Support
import           Ivory.Support.Device.GD32F4xx

data DMA_CHANNEL_IRQ
    = DMA0_Channel0
    | DMA0_Channel1
    | DMA0_Channel2
    | DMA0_Channel3
    | DMA0_Channel4
    | DMA0_Channel5
    | DMA0_Channel6
    | DMA0_Channel7
    | DMA1_Channel0
    | DMA1_Channel1
    | DMA1_Channel2
    | DMA1_Channel3
    | DMA1_Channel4
    | DMA1_Channel5
    | DMA1_Channel6
    | DMA1_Channel7
    deriving Show

data DMA_PERIPH
    = DMA0
    | DMA1
    deriving (Show, Enum, Bounded)
instance ExtDef DMA_PERIPH Uint32

data DMA_CHANNEL
    = DMA_CH0
    | DMA_CH1
    | DMA_CH2
    | DMA_CH3
    | DMA_CH4
    | DMA_CH5
    | DMA_CH6
    | DMA_CH7
    deriving (Show, Enum, Bounded)
instance ExtDef DMA_CHANNEL Uint32

data DMA_DIRECTION
    = DMA_PERIPH_TO_MEMORY
    | DMA_MEMORY_TO_PERIPH
    | DMA_MEMORY_TO_MEMORY
    deriving (Show, Enum, Bounded)
instance ExtDef DMA_DIRECTION Uint32

data DMA_MEMORY_INC
    = DMA_MEMORY_INCREASE_DISABLE
    | DMA_MEMORY_INCREASE_ENABLE
    deriving (Show, Enum, Bounded)
instance ExtDef DMA_MEMORY_INC Uint32

data DMA_MEMORY_WIDTH
    = DMA_MEMORY_WIDTH_8BIT
    | DMA_MEMORY_WIDTH_16BIT
    | DMA_MEMORY_WIDTH_32BIT
    deriving (Show, Enum, Bounded)
instance ExtDef DMA_MEMORY_WIDTH Uint32

data DMA_PERIPH_INC
    = DMA_PERIPH_INCREASE_DISABLE
    | DMA_PERIPH_INCREASE_ENABLE
    | DMA_PERIPH_INCREASE_FIX
    deriving (Show, Enum, Bounded)
instance ExtDef DMA_PERIPH_INC Uint32

data DMA_PERIPH_WIDTH
    = DMA_PERIPH_WIDTH_8BIT
    | DMA_PERIPH_WIDTH_16BIT
    | DMA_PERIPH_WIDTH_32BIT
    deriving (Show, Enum, Bounded)
instance ExtDef DMA_PERIPH_WIDTH Uint32

data DMA_PRIORITY
    = DMA_PRIORITY_LOW
    | DMA_PRIORITY_MEDIUM
    | DMA_PRIORITY_HIGH
    | DMA_PRIORITY_ULTRA_HIGH
    deriving (Show, Enum, Bounded)
instance ExtDef DMA_PRIORITY Uint32

data DMA_INT_FLAG
    = DMA_INTF_FTF
    deriving (Show, Enum, Bounded)
instance ExtDef DMA_INT_FLAG Uint32

data DMA_INT
    = DMA_CHXCTL_FTFIE
    deriving (Show, Enum, Bounded)
instance ExtDef DMA_INT Uint32


data DMA_CIRCULAR_MODE
    = DMA_CIRCULAR_MODE_ENABLE
    | DMA_CIRCULAR_MODE_DISABLE
    deriving (Show, Enum, Bounded)
instance ExtDef DMA_CIRCULAR_MODE Uint32

data DMA_SINGLE_PARAM = DMA_SINGLE_PARAM
        { dmaPeriphAddr         :: Uint32
        , dmaPeriphInc          :: DMA_PERIPH_INC
        , dmaMemoryAddr         :: Uint32
        , dmaMemoryInc          :: DMA_MEMORY_INC
        , dmaPeriphMemoryWidth  :: DMA_PERIPH_WIDTH
        , dmaCircularMode       :: DMA_CIRCULAR_MODE
        , dmaDirection          :: DMA_DIRECTION
        , dmaNumber             :: Uint32
        , dmaPriority           :: DMA_PRIORITY
        }

[ivory|
    struct dma_single_data_parameter_struct
        { periph_addr           ::  Uint32
        ; periph_inc            ::  Uint32
        ; memory0_addr          ::  Uint32
        ; memory_inc            ::  Uint32
        ; periph_memory_width   ::  Uint32
        ; circular_mode         ::  Uint32
        ; direction             ::  Uint32
        ; number                ::  Uint32
        ; priority              ::  Uint32
        }
|]

inclDMA :: ModuleM ()
inclDMA = do
    inclDef (def :: Cast DMA_CHANNEL Uint32)
    inclDef (def :: Cast DMA_PERIPH Uint32)
    inclDef (def :: Cast DMA_DIRECTION Uint32)
    inclDef (def :: Cast DMA_MEMORY_INC Uint32)
    inclDef (def :: Cast DMA_MEMORY_WIDTH Uint32)
    inclDef (def :: Cast DMA_PERIPH_INC Uint32)
    inclDef (def :: Cast DMA_PERIPH_WIDTH Uint32)
    inclDef (def :: Cast DMA_PRIORITY Uint32)
    inclDef (def :: Cast DMA_INT Uint32)
    inclDef (def :: Cast DMA_INT_FLAG Uint32)
    inclDef (def :: Cast DMA_CIRCULAR_MODE Uint32)
    incl dma_deinit
    incl dma_single_data_mode_init
    incl dma_channel_enable
    incl dma_interrupt_enable
    incl dma_interrupt_flag_get
    incl dma_interrupt_flag_clear


deinitDMA :: DMA_PERIPH -> DMA_CHANNEL -> Ivory eff ()
deinitDMA p c = call_ dma_deinit (def p) (def c)

dma_deinit :: Def ('[Uint32, Uint32] :-> ())
dma_deinit = fun "dma_deinit"


initSingleDMA :: DMA_PERIPH -> DMA_CHANNEL -> DMA_SINGLE_PARAM -> Ivory (ProcEffects s ())    ()
initSingleDMA per c p = do
    r <- local $ istruct
        [ periph_addr           .= ival (p & dmaPeriphAddr)
        , periph_inc            .= ival (p & dmaPeriphInc & def)
        , memory0_addr          .= ival (p & dmaMemoryAddr)
        , memory_inc            .= ival (p & dmaMemoryInc & def)
        , periph_memory_width   .= ival (p & dmaPeriphMemoryWidth & def)
        , circular_mode         .= ival (p & dmaCircularMode & def)
        , direction             .= ival (p & dmaDirection & def)
        , number                .= ival (p & dmaNumber)
        , priority              .= ival (p & dmaPriority & def)
        ]
    call_ dma_single_data_mode_init (def per) (def c) r

dma_single_data_mode_init ::    Def ('[Uint32, Uint32, Ref s (Struct "dma_single_data_parameter_struct")] :-> ())
dma_single_data_mode_init = fun "dma_single_data_mode_init"


enableChannelDMA :: DMA_PERIPH -> DMA_CHANNEL -> Ivory eff ()
enableChannelDMA p c = call_ dma_channel_enable (def p) (def c)

dma_channel_enable :: Def ('[Uint32, Uint32] :-> ())
dma_channel_enable = fun "dma_channel_enable"


enableInterruptDMA :: DMA_PERIPH -> DMA_CHANNEL -> DMA_INT -> Ivory eff ()
enableInterruptDMA p c i = call_ dma_interrupt_enable (def p) (def c) (def i)

dma_interrupt_enable :: Def ('[Uint32, Uint32, Uint32] :-> ())
dma_interrupt_enable = fun "dma_interrupt_enable"


getInterruptFlagDMA :: DMA_PERIPH -> DMA_CHANNEL -> DMA_INT_FLAG -> Ivory eff IBool
getInterruptFlagDMA p c f = call dma_interrupt_flag_get (def p) (def c) (def f)

dma_interrupt_flag_get :: Def ('[Uint32, Uint32, Uint32] :-> IBool)
dma_interrupt_flag_get = fun "dma_interrupt_flag_get"


clearInterruptFlagDMA :: DMA_PERIPH -> DMA_CHANNEL -> DMA_INT_FLAG -> Ivory eff ()
clearInterruptFlagDMA p c f = call_ dma_interrupt_flag_clear (def p) (def c) (def f)

dma_interrupt_flag_clear :: Def ('[Uint32, Uint32, Uint32] :-> ())
dma_interrupt_flag_clear = fun "dma_interrupt_flag_clear"


dmaParam :: DMA_SINGLE_PARAM
dmaParam =  DMA_SINGLE_PARAM 0
                             DMA_PERIPH_INCREASE_DISABLE
                             0
                             DMA_MEMORY_INCREASE_DISABLE
                             DMA_PERIPH_WIDTH_8BIT
                             DMA_CIRCULAR_MODE_DISABLE
                             DMA_PERIPH_TO_MEMORY
                             0
                             DMA_PRIORITY_LOW
