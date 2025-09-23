{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.System (
    system_core_clock,
    inclSystem,
) where

import Ivory.Language
import Ivory.Support.Device.GD32F3x0

system_core_clock = ext "SystemCoreClock" :: Uint32

inclSystem :: ModuleDef
inclSystem = do
    inclSym system_core_clock
