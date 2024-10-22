{-# LANGUAGE RecordWildCards #-}

module Device.GD32F4xx.Flash where


import           Interface.Flash
import           Ivory.Language
import           Support.Cast
import           Support.CMSIS.CoreCMFunc
import           Support.Device.GD32F4xx.FMC



data PageAddr = PageAddr
    { base   :: Uint32
    , sector :: FMC_SECTOR
    }



mkPage :: Uint32 -> FMC_SECTOR -> PageAddr
mkPage =  PageAddr



instance Flash PageAddr where
    address PageAddr{..} (Addr offset) = base + offset

    write page offset value = do
        unlockFMC
        clearFlagFMC fmc_flag_end
        clearFlagFMC fmc_flag_operr
        clearFlagFMC fmc_flag_wperr
        clearFlagFMC fmc_flag_pgmerr
        clearFlagFMC fmc_flag_pgserr
        programWordFMC (address page offset) value
        -- clearFlagFMC fmc_flag_end
        -- clearFlagFMC fmc_flag_operr
        -- clearFlagFMC fmc_flag_wperr
        -- clearFlagFMC fmc_flag_pgmerr
        -- clearFlagFMC fmc_flag_pgserr
        lockFMC

    erasePage PageAddr{..}  _ = do
        unlockFMC
        clearFlagFMC fmc_flag_end
        clearFlagFMC fmc_flag_operr
        clearFlagFMC fmc_flag_wperr
        clearFlagFMC fmc_flag_pgmerr
        clearFlagFMC fmc_flag_pgserr
        eraseSectorFMC sector
        -- clearFlagFMC fmc_flag_end
        -- clearFlagFMC fmc_flag_operr
        -- clearFlagFMC fmc_flag_wperr
        -- clearFlagFMC fmc_flag_pgmerr
        -- clearFlagFMC fmc_flag_pgserr
        lockFMC

    read page offset =
        derefUint32 $ address page offset
