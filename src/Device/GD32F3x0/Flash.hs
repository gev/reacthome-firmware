module Device.GD32F3x0.Flash where

import Interface.Flash
import Ivory.Language
import Support.Cast
import Support.Device.GD32F3x0.FMC

newtype PageAddr = PageAddr {getAddr :: Uint32}

mkPage :: Uint32 -> PageAddr
mkPage = PageAddr

instance Flash PageAddr where
    address (PageAddr page) (Addr offset) = page + offset

    write page offset value = do
        unlockFMC
        -- clearFlagFMC fmc_flag_end
        -- clearFlagFMC fmc_flag_wperr
        -- clearFlagFMC fmc_flag_pgerr
        programWordFMC (address page offset) value

    -- clearFlagFMC fmc_flag_end
    -- clearFlagFMC fmc_flag_wperr
    -- clearFlagFMC fmc_flag_pgerr
    -- lockFMC

    erasePage page offset = do
        unlockFMC
        -- clearFlagFMC fmc_flag_end
        -- clearFlagFMC fmc_flag_wperr
        -- clearFlagFMC fmc_flag_pgerr
        erasePageFMC (address page offset)

    -- clearFlagFMC fmc_flag_end
    -- clearFlagFMC fmc_flag_wperr
    -- clearFlagFMC fmc_flag_pgerr
    -- lockFMC

    read page offset =
        derefUint32 $ address page offset
