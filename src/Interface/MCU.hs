{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module Interface.MCU where

import           Core.Context
import           Interface.Mac
import           Interface.SystemClock


data MCU p = Include p => MCU
    { family       :: String
    , model        :: String
    , modification :: String
    , hasFPU       :: Bool
    , systemClock  :: SystemClock
    , peripherals  :: p
    , mac          :: Mac
    }


mcu :: Include p => String -> Bool -> SystemClock -> (String -> Mac) -> p -> String -> String -> MCU p
mcu family hasFPU systemClock initializeMac peripherals model modification = MCU
    { family       = family
    , model        = model
    , modification = modification
    , hasFPU       = hasFPU
    , systemClock  = systemClock
    , peripherals  = peripherals
    , mac          = initializeMac "mac"
    }


instance Include (MCU p) where
    include (MCU {..}) = do
        include mac
        include peripherals
        include systemClock
