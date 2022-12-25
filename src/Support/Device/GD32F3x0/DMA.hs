{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE RankNTypes            #-}

module Support.Device.GD32F3x0.DMA
( DMA_CHANNEL_IRQ   (..)
, DMA_CHANNEL       (..)
, DMA_DIRECTION     (..)
, DMA_MEMORY_INC    (..)
, DMA_MEMORY_WIDTH  (..)
, DMA_PERIPH_INC    (..)
, DMA_PERIPH_WIDTH  (..)
, DMA_PRIORITY      (..)
, DMA_PARAM         (..)
, DMA_INT           (..)
, DMA_INT_FLAG      (..)
, deinitDMA
, initDMA
, disableCirculationDMA
, disableMemoryToMemoryDMA
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
import           Ivory.Support.Device.GD32F3x0

data DMA_CHANNEL_IRQ
  = DMA_Channel3_4
  deriving Show

data DMA_CHANNEL
  = DMA_CH0
  | DMA_CH1
  | DMA_CH2
  | DMA_CH3
  | DMA_CH4
  | DMA_CH5
  | DMA_CH6
  deriving (Show, Enum, Bounded)
instance ExtDef DMA_CHANNEL Uint32

data DMA_DIRECTION
  = DMA_PERIPHERAL_TO_MEMORY
  | DMA_MEMORY_TO_PERIPHERAL
  deriving (Show, Enum, Bounded)
instance ExtDef DMA_DIRECTION Uint8

data DMA_MEMORY_INC
  = DMA_MEMORY_INCREASE_DISABLE
  | DMA_MEMORY_INCREASE_ENABLE
  deriving (Show, Enum, Bounded)
instance ExtDef DMA_MEMORY_INC Uint8

data DMA_MEMORY_WIDTH
  = DMA_MEMORY_WIDTH_8BIT
  | DMA_MEMORY_WIDTH_16BIT
  | DMA_MEMORY_WIDTH_32BIT
  deriving (Show, Enum, Bounded)
instance ExtDef DMA_MEMORY_WIDTH Uint32

data DMA_PERIPH_INC
  = DMA_PERIPH_INCREASE_DISABLE
  | DMA_PERIPH_INCREASE_ENABLE
  deriving (Show, Enum, Bounded)
instance ExtDef DMA_PERIPH_INC Uint8

data DMA_PERIPH_WIDTH
  = DMA_PERIPHERAL_WIDTH_8BIT
  | DMA_PERIPHERAL_WIDTH_16BIT
  | DMA_PERIPHERAL_WIDTH_32BIT
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
  = DMA_INT_FLAG_FTF
  | DMA_INT_FLAG_G
  deriving (Show, Enum, Bounded)
instance ExtDef DMA_INT_FLAG Uint32

data DMA_INT
  = DMA_INT_FTF
  deriving (Show, Enum, Bounded)
instance ExtDef DMA_INT Uint32

data DMA_PARAM = DMA_PARAM
    { dmaPeriphAddr  :: Uint32
    , dmaPeriphWidth :: DMA_PERIPH_WIDTH
    , dmaPeriphInc   :: DMA_PERIPH_INC
    , dmaMemoryAddr  :: Uint32
    , dmaMemoryWidth :: DMA_MEMORY_WIDTH
    , dmaMemoryInc   :: DMA_MEMORY_INC
    , dmaDirection   :: DMA_DIRECTION
    , dmaNumber      :: Sint32
    , dmaPriority    :: DMA_PRIORITY
    }

[ivory|
  struct dma_parameter_struct
    { periph_addr    :: Uint32
    ; periph_width   :: Uint32
    ; periph_inc     :: Uint8
    ; memory_addr    :: Uint32
    ; memory_width   :: Uint32
    ; memory_inc     :: Uint8
    ; direction      :: Uint8
    ; number         :: Sint32
    ; priority       :: Uint32
    }
|]

inclDMA :: ModuleM ()
inclDMA = do
  inclDef (def :: Cast DMA_CHANNEL Uint32)
  inclDef (def :: Cast DMA_DIRECTION Uint8)
  inclDef (def :: Cast DMA_MEMORY_INC Uint8)
  inclDef (def :: Cast DMA_MEMORY_WIDTH Uint32)
  inclDef (def :: Cast DMA_PERIPH_INC Uint8)
  inclDef (def :: Cast DMA_PERIPH_WIDTH Uint32)
  inclDef (def :: Cast DMA_PRIORITY Uint32)
  inclDef (def :: Cast DMA_INT Uint32)
  inclDef (def :: Cast DMA_INT_FLAG Uint32)
  incl dma_deinit
  incl dma_init
  incl dma_circulation_disable
  incl dma_memory_to_memory_disable
  incl dma_channel_enable
  incl dma_interrupt_enable
  incl dma_interrupt_flag_get
  incl dma_interrupt_flag_clear


deinitDMA :: DMA_CHANNEL -> Ivory eff ()
deinitDMA = call_ dma_deinit . def

dma_deinit :: Def ('[Uint32] :-> ())
dma_deinit = fun "dma_deinit"


initDMA :: DMA_CHANNEL -> DMA_PARAM -> Ivory (ProcEffects s ())  ()
initDMA c p = do
  r <- local $ istruct
    [ periph_addr      .=  ival (p & dmaPeriphAddr)
    , periph_width     .=  ival (p & dmaPeriphWidth & def)
    , periph_inc       .=  ival (p & dmaPeriphInc & def)
    , memory_addr      .=  ival (p & dmaMemoryAddr)
    , memory_width     .=  ival (p & dmaMemoryWidth & def)
    , memory_inc       .=  ival (p & dmaMemoryInc & def)
    , direction        .=  ival (p & dmaDirection & def)
    , number           .=  ival (p & dmaNumber)
    , priority         .=  ival (p & dmaPriority & def)
    ]
  call_ dma_init (def c) r

dma_init ::  Def ('[Uint32, Ref s (Struct "dma_parameter_struct")] :-> ())
dma_init = fun "dma_init"


disableCirculationDMA :: DMA_CHANNEL -> Ivory eff ()
disableCirculationDMA = call_ dma_circulation_disable . def

dma_circulation_disable :: Def ('[Uint32] :-> ())
dma_circulation_disable = fun "dma_circulation_disable"


disableMemoryToMemoryDMA :: DMA_CHANNEL -> Ivory eff ()
disableMemoryToMemoryDMA = call_ dma_memory_to_memory_disable . def

dma_memory_to_memory_disable :: Def ('[Uint32] :-> ())
dma_memory_to_memory_disable = fun "dma_memory_to_memory_disable"


enableChannelDMA :: DMA_CHANNEL -> Ivory eff ()
enableChannelDMA = call_ dma_channel_enable . def

dma_channel_enable :: Def ('[Uint32] :-> ())
dma_channel_enable = fun "dma_channel_enable"


enableInterruptDMA :: DMA_CHANNEL -> DMA_INT -> Ivory eff ()
enableInterruptDMA c i = call_ dma_interrupt_enable (def c) (def i)

dma_interrupt_enable :: Def ('[Uint32, Uint32] :-> ())
dma_interrupt_enable = fun "dma_interrupt_enable"


getInterruptFlagDMA :: DMA_CHANNEL -> DMA_INT_FLAG -> Ivory eff IBool
getInterruptFlagDMA c f = call dma_interrupt_flag_get (def c) (def f)

dma_interrupt_flag_get :: Def ('[Uint32, Uint32] :-> IBool)
dma_interrupt_flag_get = fun "dma_interrupt_flag_get"


clearInterruptFlagDMA :: DMA_CHANNEL -> DMA_INT_FLAG -> Ivory eff ()
clearInterruptFlagDMA c f = call_ dma_interrupt_flag_clear (def c) (def f)

dma_interrupt_flag_clear :: Def ('[Uint32, Uint32] :-> ())
dma_interrupt_flag_clear = fun "dma_interrupt_flag_clear"


dmaParam :: DMA_PARAM
dmaParam =  DMA_PARAM 0
                      DMA_PERIPHERAL_WIDTH_8BIT
                      DMA_PERIPH_INCREASE_DISABLE
                      0
                      DMA_MEMORY_WIDTH_8BIT
                      DMA_MEMORY_INCREASE_DISABLE
                      DMA_PERIPHERAL_TO_MEMORY
                      0
                      DMA_PRIORITY_LOW
