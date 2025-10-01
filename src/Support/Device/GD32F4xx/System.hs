{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F4xx.System (
    system_core_clock,
    inclSystem,
) where

import Ivory.Language
import Ivory.Support.Device.GD32F4xx

system_core_clock = ext "SystemCoreClock" :: Uint32

inclSystem :: ModuleDef
inclSystem = do
    inclSym system_core_clock
