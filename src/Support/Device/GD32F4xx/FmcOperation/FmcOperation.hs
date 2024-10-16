{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F4xx.FmcOperation.FmcOperation
    ( eraseSectorByAddress
    , write32Bit

    , inclFmcOperation
    ) where

import           Ivory.Language
import           Ivory.Support
import           Ivory.Support.Device.GD32F4xx


eraseSectorByAddress :: Uint32 -> Ivory eff ()
eraseSectorByAddress = call_ fmc_erase_sector_by_address

fmc_erase_sector_by_address :: Def ('[Uint32] :-> ())
fmc_erase_sector_by_address = fun "fmc_erase_sector_by_address"


write32Bit :: Uint32 -> Uint32 -> Ivory eff ()
write32Bit =  call_ fmc_write_32bit

fmc_write_32bit :: Def ('[Uint32, Uint32] :-> ())
fmc_write_32bit = fun "fmc_write_32bit"


inclFmcOperation :: ModuleDef
inclFmcOperation = do

    incl fmc_erase_sector_by_address
    incl fmc_write_32bit
