{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Support.Device.GD32F4xx.SPI
    ( SPI_PERIPH
    , spi1
    , spi2

    , I2S_MODE
    , i2s_mode_mastertx

    , I2S_STANDARD
    , i2s_std_phillips

    , I2S_CKPL
    , i2s_ckpl_low

    , I2S_AUDIOSAMPLE
    , i2s_audiosample_48k

    , I2S_FRAMEFORMAT
    , i2s_frameformat_dt16b_ch16b
    , i2s_frameformat_dt32b_ch32b

    , I2S_MCKOUT
    , i2s_mckout_enable

    , deinitSPII2S
    , initI2S
    , configPscI2S
    , configFullDuplexModeI2S
    , dataSpi

    , inclSPI
    ) where

import           Ivory.Language
import           Ivory.Support
import           Ivory.Support.Device.GD32F4xx

newtype SPI_PERIPH = SPI_PERIPH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
instance ExtSymbol SPI_PERIPH

spi1 = SPI_PERIPH $ ext "SPI1"
spi2 = SPI_PERIPH $ ext "SPI2"


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
i2s_frameformat_dt32b_ch32b = I2S_FRAMEFORMAT $ ext "I2S_FRAMEFORMAT_DT32B_CH32B"


newtype I2S_MCKOUT = I2S_MCKOUT Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)    

i2s_mckout_enable = I2S_MCKOUT $ ext "I2S_MCKOUT_ENABLE"



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


dataSpi :: SPI_PERIPH -> Ivory eff Uint32
dataSpi = call spi_data

spi_data :: Def ('[SPI_PERIPH] :-> Uint32)
spi_data = fun "(uint32_t)&SPI_DATA"



inclSPI :: ModuleDef
inclSPI = do
    incl spi_i2s_deinit
    incl i2s_init
    incl i2s_psc_config
    incl i2s_full_duplex_mode_config
    incl spi_data


    inclSym spi1
    inclSym spi2

    inclSym i2s_std_phillips

    inclSym i2s_ckpl_low

    inclSym i2s_audiosample_48k

    inclSym i2s_frameformat_dt16b_ch16b
    inclSym i2s_frameformat_dt32b_ch32b

    inclSym i2s_mckout_enable
