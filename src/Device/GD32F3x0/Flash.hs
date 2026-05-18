module Device.GD32F3x0.Flash where

import Interface.Flash
import Ivory.Language
import Ivory.Stdlib
import Support.Cast
import Support.Device.GD32F3x0.FMC

mkFlash = Flash{read, write, erase}
 where
  read = derefUint32

  write offset value = do
    unlockFMC
    -- clearFlagFMC fmc_flag_end
    -- clearFlagFMC fmc_flag_wperr
    -- clearFlagFMC fmc_flag_pgerr
    programWordFMC offset value
  -- clearFlagFMC fmc_flag_end
  -- clearFlagFMC fmc_flag_wperr
  -- clearFlagFMC fmc_flag_pgerr
  -- lockFMC

  erase offset = do
    when (offset .% 0x400 ==? 0) do
      unlockFMC
      -- clearFlagFMC fmc_flag_end
      -- clearFlagFMC fmc_flag_wperr
      -- clearFlagFMC fmc_flag_pgerr
      erasePageFMC offset

-- clearFlagFMC fmc_flag_end
-- clearFlagFMC fmc_flag_wperr
-- clearFlagFMC fmc_flag_pgerr
-- lockFMC
