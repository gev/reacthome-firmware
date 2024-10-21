{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
module Device.GD32F3x0.Flash where


import           Control.Monad.State
import           Core.Context
import           Interface.Flash             hiding (getAddr)
import           Ivory.Language
import           Support.Cast
import           Support.Device.GD32F3x0.FMC


newtype PageAddr = PageAddr {getAddr :: Uint32}


mkPage :: MonadState Context m => Uint32 -> m PageAddr
mkPage getAddr =
    pure PageAddr { getAddr }



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
