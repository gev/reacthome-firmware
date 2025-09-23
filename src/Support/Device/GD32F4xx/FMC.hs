{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F4xx.FMC (
    FMC_SECTOR,
    fmc_sector_0,
    fmc_sector_1,
    fmc_sector_2,
    fmc_sector_3,
    fmc_sector_4,
    fmc_sector_5,
    fmc_sector_6,
    fmc_sector_7,
    fmc_sector_8,
    fmc_sector_9,
    fmc_sector_10,
    fmc_sector_11,
    FMC_FLAG,
    fmc_flag_end,
    fmc_flag_operr,
    fmc_flag_wperr,
    fmc_flag_pgmerr,
    fmc_flag_pgserr,
    fmc_flag_rdderr,
    FMC_OB_WP,
    ob_wp_all,
    unlockFMC,
    lockFMC,
    unlockOB,
    lockOB,
    startOB,
    clearFlagFMC,
    programWordFMC,
    eraseSectorFMC,
    writeProtectionDisableOB,
    inclFMC,
) where

import Ivory.Language
import Ivory.Support
import Ivory.Support.Device.GD32F4xx

newtype FMC_SECTOR = FMC_SECTOR Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
fmc_sector_0 = FMC_SECTOR $ ext "CTL_SECTOR_NUMBER_0"
fmc_sector_1 = FMC_SECTOR $ ext "CTL_SECTOR_NUMBER_1"
fmc_sector_2 = FMC_SECTOR $ ext "CTL_SECTOR_NUMBER_2"
fmc_sector_3 = FMC_SECTOR $ ext "CTL_SECTOR_NUMBER_3"
fmc_sector_4 = FMC_SECTOR $ ext "CTL_SECTOR_NUMBER_4"
fmc_sector_5 = FMC_SECTOR $ ext "CTL_SECTOR_NUMBER_5"
fmc_sector_6 = FMC_SECTOR $ ext "CTL_SECTOR_NUMBER_6"
fmc_sector_7 = FMC_SECTOR $ ext "CTL_SECTOR_NUMBER_7"
fmc_sector_8 = FMC_SECTOR $ ext "CTL_SECTOR_NUMBER_8"
fmc_sector_9 = FMC_SECTOR $ ext "CTL_SECTOR_NUMBER_9"
fmc_sector_10 = FMC_SECTOR $ ext "CTL_SECTOR_NUMBER_10"
fmc_sector_11 = FMC_SECTOR $ ext "CTL_SECTOR_NUMBER_11"

newtype FMC_FLAG = FMC_FLAG Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
fmc_flag_end = FMC_FLAG $ ext "FMC_FLAG_END"
fmc_flag_operr = FMC_FLAG $ ext "FMC_FLAG_OPERR"
fmc_flag_wperr = FMC_FLAG $ ext "FMC_FLAG_WPERR"
fmc_flag_pgmerr = FMC_FLAG $ ext "FMC_FLAG_PGMERR"
fmc_flag_pgserr = FMC_FLAG $ ext "FMC_FLAG_PGSERR"
fmc_flag_rdderr = FMC_FLAG $ ext "FMC_FLAG_RDDERR"

newtype FMC_OB_WP = FMC_OB_WP Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
ob_wp_all = FMC_OB_WP $ ext "OB_WP_ALL"

unlockFMC :: Ivory eff ()
unlockFMC = call_ fmc_unlock

fmc_unlock :: Def ('[] :-> ())
fmc_unlock = fun "fmc_unlock"

lockFMC :: Ivory eff ()
lockFMC = call_ fmc_lock

fmc_lock :: Def ('[] :-> ())
fmc_lock = fun "fmc_lock"

unlockOB :: Ivory eff ()
unlockOB = call_ ob_unlock

ob_unlock :: Def ('[] :-> ())
ob_unlock = fun "ob_unlock"

lockOB :: Ivory eff ()
lockOB = call_ ob_lock

ob_lock :: Def ('[] :-> ())
ob_lock = fun "ob_lock"

startOB :: Ivory eff ()
startOB = call_ ob_start

ob_start :: Def ('[] :-> ())
ob_start = fun "ob_start"

clearFlagFMC :: FMC_FLAG -> Ivory eff ()
clearFlagFMC = call_ fmc_flag_clear

fmc_flag_clear :: Def ('[FMC_FLAG] :-> ())
fmc_flag_clear = fun "fmc_flag_clear"

programWordFMC :: Uint32 -> Uint32 -> Ivory eff ()
programWordFMC = call_ fmc_word_program

fmc_word_program :: Def ('[Uint32, Uint32] :-> ())
fmc_word_program = fun "fmc_word_program"

eraseSectorFMC :: FMC_SECTOR -> Ivory eff ()
eraseSectorFMC = call_ fmc_sector_erase

fmc_sector_erase :: Def ('[FMC_SECTOR] :-> ())
fmc_sector_erase = fun "fmc_sector_erase"

writeProtectionDisableOB :: FMC_OB_WP -> Ivory eff ()
writeProtectionDisableOB = call_ ob_write_protection_disable

ob_write_protection_disable :: Def ('[FMC_OB_WP] :-> ())
ob_write_protection_disable = fun "ob_write_protection_disable"

inclFMC :: ModuleDef
inclFMC = do
    inclSym fmc_sector_0
    inclSym fmc_sector_1
    inclSym fmc_sector_2
    inclSym fmc_sector_3
    inclSym fmc_sector_4
    inclSym fmc_sector_5
    inclSym fmc_sector_6
    inclSym fmc_sector_7
    inclSym fmc_sector_8
    inclSym fmc_sector_9
    inclSym fmc_sector_10
    inclSym fmc_sector_11

    inclSym fmc_flag_end
    inclSym fmc_flag_operr
    inclSym fmc_flag_wperr
    inclSym fmc_flag_pgmerr
    inclSym fmc_flag_pgserr
    inclSym fmc_flag_rdderr

    inclSym ob_wp_all

    incl fmc_unlock
    incl fmc_lock
    incl ob_unlock
    incl ob_lock
    incl ob_start
    incl fmc_flag_clear
    incl fmc_word_program
    incl fmc_sector_erase
    incl ob_write_protection_disable
