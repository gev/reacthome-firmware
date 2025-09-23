{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F4xx.SPI (
    SPI_PERIPH,
    spi1,
    spi2,
    i2s1_add,
    i2s2_add,
    I2S_MODE,
    i2s_mode_mastertx,
    I2S_STANDARD,
    i2s_std_phillips,
    I2S_CKPL,
    i2s_ckpl_low,
    I2S_AUDIOSAMPLE,
    i2s_audiosample_48k,
    I2S_FRAMEFORMAT,
    i2s_frameformat_dt16b_ch16b,
    i2s_frameformat_dt24b_ch32b,
    i2s_frameformat_dt32b_ch32b,
    I2S_MCKOUT,
    i2s_mckout_enable,
    SPI_DMA_DEFINITION,
    spi_dma_transmit,
    spi_dma_receive,
    deinitSPII2S,
    initI2S,
    configPscI2S,
    configFullDuplexModeI2S,
    dataSPI,
    enableI2S,
    enableSpiDma,
    inclSPI,
) where

import Ivory.Language
import Ivory.Support
import Ivory.Support.Device.GD32F4xx

newtype SPI_PERIPH = SPI_PERIPH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
instance ExtSymbol SPI_PERIPH

spi1 = SPI_PERIPH $ ext "SPI1"
spi2 = SPI_PERIPH $ ext "SPI2"
i2s1_add = SPI_PERIPH $ ext "I2S1_ADD"
i2s2_add = SPI_PERIPH $ ext "I2S2_ADD"

newtype I2S_MODE = I2S_MODE Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

i2s_mode_mastertx = I2S_MODE $ ext "I2S_MODE_MASTERTX"

newtype I2S_STANDARD = I2S_STANDARD Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

i2s_std_phillips = I2S_STANDARD $ ext "I2S_STD_PHILLIPS"

newtype I2S_CKPL = I2S_CKPL Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

i2s_ckpl_low = I2S_CKPL $ ext "I2S_CKPL_LOW"

newtype I2S_AUDIOSAMPLE = I2S_AUDIOSAMPLE Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

i2s_audiosample_48k = I2S_AUDIOSAMPLE $ ext "I2S_AUDIOSAMPLE_48K"

newtype I2S_FRAMEFORMAT = I2S_FRAMEFORMAT Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

i2s_frameformat_dt16b_ch16b = I2S_FRAMEFORMAT $ ext "I2S_FRAMEFORMAT_DT16B_CH16B"
i2s_frameformat_dt24b_ch32b = I2S_FRAMEFORMAT $ ext "I2S_FRAMEFORMAT_DT24B_CH32B"
i2s_frameformat_dt32b_ch32b = I2S_FRAMEFORMAT $ ext "I2S_FRAMEFORMAT_DT32B_CH32B"

newtype I2S_MCKOUT = I2S_MCKOUT Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

i2s_mckout_enable = I2S_MCKOUT $ ext "I2S_MCKOUT_ENABLE"

newtype SPI_DMA_DEFINITION = SPI_DMA_DEFINITION Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

spi_dma_transmit = SPI_DMA_DEFINITION $ ext "SPI_DMA_TRANSMIT"
spi_dma_receive = SPI_DMA_DEFINITION $ ext "SPI_DMA_RECEIVE"

deinitSPII2S :: SPI_PERIPH -> Ivory eff ()
deinitSPII2S = call_ spi_i2s_deinit

spi_i2s_deinit :: Def ('[SPI_PERIPH] :-> ())
spi_i2s_deinit = fun "spi_i2s_deinit"

initI2S :: SPI_PERIPH -> I2S_MODE -> I2S_STANDARD -> I2S_CKPL -> Ivory eff ()
initI2S = call_ i2s_init

i2s_init :: Def ('[SPI_PERIPH, I2S_MODE, I2S_STANDARD, I2S_CKPL] :-> ())
i2s_init = fun "i2s_init"

configPscI2S :: SPI_PERIPH -> I2S_AUDIOSAMPLE -> I2S_FRAMEFORMAT -> I2S_MCKOUT -> Ivory eff ()
configPscI2S = call_ i2s_psc_config

i2s_psc_config :: Def ('[SPI_PERIPH, I2S_AUDIOSAMPLE, I2S_FRAMEFORMAT, I2S_MCKOUT] :-> ())
i2s_psc_config = fun "i2s_psc_config"

configFullDuplexModeI2S :: SPI_PERIPH -> I2S_MODE -> I2S_STANDARD -> I2S_CKPL -> I2S_FRAMEFORMAT -> Ivory eff ()
configFullDuplexModeI2S = call_ i2s_full_duplex_mode_config

i2s_full_duplex_mode_config :: Def ('[SPI_PERIPH, I2S_MODE, I2S_STANDARD, I2S_CKPL, I2S_FRAMEFORMAT] :-> ())
i2s_full_duplex_mode_config = fun "i2s_full_duplex_mode_config"

dataSPI :: SPI_PERIPH -> Ivory eff Uint32
dataSPI = call spi_data

spi_data :: Def ('[SPI_PERIPH] :-> Uint32)
spi_data = fun "(uint32_t)&SPI_DATA"

enableI2S :: SPI_PERIPH -> Ivory eff ()
enableI2S = call_ i2s_enable

i2s_enable :: Def ('[SPI_PERIPH] :-> ())
i2s_enable = fun "i2s_enable"

enableSpiDma :: SPI_PERIPH -> SPI_DMA_DEFINITION -> Ivory eff ()
enableSpiDma = call_ spi_dma_enable

spi_dma_enable :: Def ('[SPI_PERIPH, SPI_DMA_DEFINITION] :-> ())
spi_dma_enable = fun "spi_dma_enable"

inclSPI :: ModuleDef
inclSPI = do
    incl spi_i2s_deinit
    incl i2s_init
    incl i2s_psc_config
    incl i2s_full_duplex_mode_config
    incl spi_data
    incl i2s_enable
    incl spi_dma_enable

    inclSym spi1
    inclSym spi2
    inclSym i2s1_add
    inclSym i2s2_add

    inclSym i2s_mode_mastertx

    inclSym i2s_std_phillips

    inclSym i2s_ckpl_low

    inclSym i2s_audiosample_48k

    inclSym i2s_frameformat_dt16b_ch16b
    inclSym i2s_frameformat_dt24b_ch32b
    inclSym i2s_frameformat_dt32b_ch32b

    inclSym i2s_mckout_enable

    inclSym spi_dma_transmit
    inclSym spi_dma_receive
