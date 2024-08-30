
module Device.GD32F4xx.Flash where


import           Interface.Flash
import           Ivory.Language
import           Support.Cast
import           Support.Device.GD32F4xx.FMC


newtype PageAddr = PageAddr {getAddr :: Uint32}


mkPage :: Uint32 -> PageAddr
mkPage =  PageAddr



instance Flash PageAddr where
    address (PageAddr page) (Addr offset) = page + offset

    write page offset value = do
        unlockFMC
        clearFlagFMC fmc_flag_end
        clearFlagFMC fmc_flag_operr
        clearFlagFMC fmc_flag_wperr
        clearFlagFMC fmc_flag_pgmerr
        clearFlagFMC fmc_flag_pgserr
        programWordFMC (address page offset) value
        lockFMC

    erasePage (PageAddr page) offset = do
        unlockFMC
        clearFlagFMC fmc_flag_end
        clearFlagFMC fmc_flag_operr
        clearFlagFMC fmc_flag_wperr
        clearFlagFMC fmc_flag_pgmerr
        clearFlagFMC fmc_flag_pgserr
        eraseSectorFMC page
        lockFMC

    read page offset =
        derefUint32 $ address page offset
