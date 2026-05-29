module Device.GD32F4xx.Flash where

import Interface.Flash
import Ivory.Stdlib
import Support.Cast
import Support.Device.GD32F4xx.FMC
import Ivory.Language
import Support.CMSIS.CoreCMFunc (disableIRQ, enableIRQ)

mkFlash = Flash{read, write, erase}
  where
    read = derefUint32

    write offset value = do
        disableIRQ
        unlockFMC
        clearFlagFMC fmc_flag_end
        clearFlagFMC fmc_flag_operr
        clearFlagFMC fmc_flag_wperr
        clearFlagFMC fmc_flag_pgmerr
        clearFlagFMC fmc_flag_pgserr
        programWordFMC offset value
        clearFlagFMC fmc_flag_end
        clearFlagFMC fmc_flag_operr
        clearFlagFMC fmc_flag_wperr
        clearFlagFMC fmc_flag_pgmerr
        clearFlagFMC fmc_flag_pgserr
        lockFMC
        enableIRQ

    erase offset = do
        cond_
            [ offset ==? 0x0800_0000 ==> eraseSector fmc_sector_0
            , offset ==? 0x0800_4000 ==> eraseSector fmc_sector_1
            , offset ==? 0x0800_8000 ==> eraseSector fmc_sector_2
            , offset ==? 0x0800_C000 ==> eraseSector fmc_sector_3
            , offset ==? 0x0801_0000 ==> eraseSector fmc_sector_4
            , offset ==? 0x0802_0000 ==> eraseSector fmc_sector_5
            , offset ==? 0x0804_0000 ==> eraseSector fmc_sector_6
            , offset ==? 0x0806_0000 ==> eraseSector fmc_sector_7
            , offset ==? 0x0808_0000 ==> eraseSector fmc_sector_8
            , offset ==? 0x080A_0000 ==> eraseSector fmc_sector_9
            , offset ==? 0x080C_0000 ==> eraseSector fmc_sector_10
            , offset ==? 0x080E_0000 ==> eraseSector fmc_sector_11
            , offset ==? 0x0820_0000 ==> eraseSector fmc_sector_24
            , offset ==? 0x0824_0000 ==> eraseSector fmc_sector_25
            , offset ==? 0x0828_0000 ==> eraseSector fmc_sector_26
            , offset ==? 0x082C_0000 ==> eraseSector fmc_sector_27
            ]
      where
        eraseSector sector = do
            disableIRQ
            unlockFMC
            clearFlagFMC fmc_flag_end
            clearFlagFMC fmc_flag_operr
            clearFlagFMC fmc_flag_wperr
            clearFlagFMC fmc_flag_pgmerr
            clearFlagFMC fmc_flag_pgserr
            eraseSectorFMC sector
            clearFlagFMC fmc_flag_end
            clearFlagFMC fmc_flag_operr
            clearFlagFMC fmc_flag_wperr
            clearFlagFMC fmc_flag_pgmerr
            clearFlagFMC fmc_flag_pgserr
            lockFMC
            enableIRQ

