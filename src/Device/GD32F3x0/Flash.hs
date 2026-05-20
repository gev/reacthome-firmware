module Device.GD32F3x0.Flash where

import Interface.Flash
import Ivory.Language
import Ivory.Stdlib
import Support.CMSIS.CoreCMFunc
import Support.Cast
import Support.Device.GD32F3x0.FMC

mkFlash base = Flash{base, read, write, erase}
  where
    read = derefUint32

    write offset value = do
        disableIRQ
        unlockFMC
        clearFlagFMC fmc_flag_end
        clearFlagFMC fmc_flag_wperr
        clearFlagFMC fmc_flag_pgerr
        programWordFMC offset value
        clearFlagFMC fmc_flag_end
        clearFlagFMC fmc_flag_wperr
        clearFlagFMC fmc_flag_pgerr
        lockFMC
        enableIRQ

    erase offset = do
        when (offset .% 0x400 ==? 0) do
            disableIRQ
            unlockFMC
            clearFlagFMC fmc_flag_end
            clearFlagFMC fmc_flag_wperr
            clearFlagFMC fmc_flag_pgerr
            erasePageFMC offset
            clearFlagFMC fmc_flag_end
            clearFlagFMC fmc_flag_wperr
            clearFlagFMC fmc_flag_pgerr
            lockFMC
            enableIRQ
