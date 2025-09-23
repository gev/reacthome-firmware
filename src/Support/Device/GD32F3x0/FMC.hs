{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Support.Device.GD32F3x0.FMC (
    FMC_FLAG,
    fmc_flag_end,
    fmc_flag_wperr,
    fmc_flag_pgerr,
    unlockFMC,
    lockFMC,
    clearFlagFMC,
    programWordFMC,
    erasePageFMC,
    inclFMC,
) where

import Ivory.Language
import Ivory.Support
import Ivory.Support.Device.GD32F3x0

newtype FMC_FLAG = FMC_FLAG Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
fmc_flag_end = FMC_FLAG $ ext "FMC_FLAG_END"
fmc_flag_wperr = FMC_FLAG $ ext "FMC_FLAG_WPERR"
fmc_flag_pgerr = FMC_FLAG $ ext "FMC_FLAG_PGERR"

unlockFMC :: Ivory eff ()
unlockFMC = call_ fmc_unlock

fmc_unlock :: Def ('[] :-> ())
fmc_unlock = fun "fmc_unlock"

lockFMC :: Ivory eff ()
lockFMC = call_ fmc_lock

fmc_lock :: Def ('[] :-> ())
fmc_lock = fun "fmc_lock"

clearFlagFMC :: FMC_FLAG -> Ivory eff ()
clearFlagFMC = call_ fmc_flag_clear

fmc_flag_clear :: Def ('[FMC_FLAG] :-> ())
fmc_flag_clear = fun "fmc_flag_clear"

programWordFMC :: Uint32 -> Uint32 -> Ivory eff ()
programWordFMC = call_ fmc_word_program

fmc_word_program :: Def ('[Uint32, Uint32] :-> ())
fmc_word_program = fun "fmc_word_program"

erasePageFMC :: Uint32 -> Ivory eff ()
erasePageFMC = call_ fmc_page_erase

fmc_page_erase :: Def ('[Uint32] :-> ())
fmc_page_erase = fun "fmc_page_erase"

inclFMC :: ModuleDef
inclFMC = do
    inclSym fmc_flag_end
    inclSym fmc_flag_wperr
    inclSym fmc_flag_pgerr

    incl fmc_unlock
    incl fmc_lock
    incl fmc_flag_clear
    incl fmc_word_program
    incl fmc_page_erase
