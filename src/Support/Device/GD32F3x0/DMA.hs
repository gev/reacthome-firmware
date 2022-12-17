{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}

module Support.Device.GD32F3x0.DMA
(

) where

import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Support
import           Ivory.Support.Device.GD32F3x0

data DMA_DIRECTION
  = DMA_PERIPHERAL_TO_MEMORY
  | DMA_MEMORY_TO_PERIPHERAL
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
    deriving (Show, Enum, Bounded)
instance ExtDef DMA_PERIPH_INC Uint32

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



data DMA_PARAM = DMA_PARAM
    { dmaPeriphAddr  :: Uint32
    , dmaPeriphWidth :: DMA_PERIPH_WIDTH
    , dmaPeriphInc   :: DMA_PERIPH_INC
    , dmaMemoryAddr  :: Uint32
    , dmaMemoryWidth :: DMA_MEMORY_WIDTH
    , dmaMemoryInc   :: DMA_MEMORY_INC
    , dmaDirection   :: DMA_DIRECTION
    , dmaNumber      :: Uint32
    , dmaPriority    :: DMA_PRIORITY
    }

[ivory|
  struct dma_parameter_struct
    { periph_addr       :: Stored Uint32
    ; periph_width      :: Stored Uint32
    ; periph_inc        :: Stored Uint8
    ; memory_addr       :: Stored Uint32
    ; memory_width      :: Stored Uint32
    ; memory_inc        :: Stored Uint8
    ; direction         :: Stored Uint8
    ; number            :: Stored Uint32
    ; priority          :: Stored Uint32
    }
|]

inclDMA :: [ ModuleM () ]
inclDMA =  [ inclDef (def :: Cast DMA_DIRECTION Uint32)
           , inclDef (def :: Cast DMA_MEMORY_INC Uint32)
           , inclDef (def :: Cast DMA_MEMORY_WIDTH Uint32)
           , inclDef (def :: Cast DMA_PERIPH_INC Uint32)
           , inclDef (def :: Cast DMA_PERIPH_WIDTH Uint32)
           , inclDef (def :: Cast DMA_PRIORITY Uint32)
          --  , incl dma_deinit
          --  , incl dma_init
          --  , incl dma_circulation_disable
          --  , incl dma_memory_to_memory_disable
          --  , incl dma_channel_enable
          --  , incl dma_flag_get
           ]
