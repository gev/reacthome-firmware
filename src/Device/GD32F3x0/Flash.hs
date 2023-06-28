
module Device.GD32F3x0.Flash where


import           Interface.Flash
import           Ivory.Language
import           Support.Device.GD32F3x0.FMC
import           Support.Cast


newtype BaseAddr = BaseAddr {getBase :: Uint32}


instance Flash BaseAddr where
    address (BaseAddr base) (Addr offset) = base + offset

    writeFlash base offset value = do
        unlockFMC
        clearFlagFMC fmc_flag_end
        clearFlagFMC fmc_flag_wperr
        clearFlagFMC fmc_flag_pgerr
        programWordFMC (address base offset) value
        clearFlagFMC fmc_flag_end
        clearFlagFMC fmc_flag_wperr
        clearFlagFMC fmc_flag_pgerr
        lockFMC

    erasePage (BaseAddr base) = do 
        unlockFMC
        clearFlagFMC fmc_flag_end
        clearFlagFMC fmc_flag_wperr
        clearFlagFMC fmc_flag_pgerr
        erasePageFMC base
        clearFlagFMC fmc_flag_end
        clearFlagFMC fmc_flag_wperr
        clearFlagFMC fmc_flag_pgerr
        lockFMC

    readFlash base offset = 
        derefUint32 $ address base offset





