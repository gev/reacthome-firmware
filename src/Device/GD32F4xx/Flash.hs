module Device.GD32F4xx.Flash where

import Interface.Flash
import Support.Cast
import Support.Device.GD32F4xx.FMC

mkFlash = Flash{read, write, erase}
  where
    read = derefUint32

    write offset value = do
        unlockFMC
        clearFlagFMC fmc_flag_end
        clearFlagFMC fmc_flag_operr
        clearFlagFMC fmc_flag_wperr
        clearFlagFMC fmc_flag_pgmerr
        clearFlagFMC fmc_flag_pgserr
        programWordFMC offset value
        -- clearFlagFMC fmc_flag_end
        -- clearFlagFMC fmc_flag_operr
        -- clearFlagFMC fmc_flag_wperr
        -- clearFlagFMC fmc_flag_pgmerr
        -- clearFlagFMC fmc_flag_pgserr
        lockFMC

    erase offset = do
        unlockFMC
        clearFlagFMC fmc_flag_end
        clearFlagFMC fmc_flag_operr
        clearFlagFMC fmc_flag_wperr
        clearFlagFMC fmc_flag_pgmerr
        clearFlagFMC fmc_flag_pgserr
        {-ToDo convert offset into sector-}
        eraseSectorFMC $ sector offset
        -- clearFlagFMC fmc_flag_end
        -- clearFlagFMC fmc_flag_operr
        -- clearFlagFMC fmc_flag_wperr
        -- clearFlagFMC fmc_flag_pgmerr
        -- clearFlagFMC fmc_flag_pgserr
        lockFMC

    sector = undefined
