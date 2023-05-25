{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Support.Device.GD32F3x0.System
    (CORE_CLOCK
    , system_core_clock
    ) where

import           Ivory.Language
import           Ivory.Support.Device.GD32F3x0

newtype CORE_CLOCK = CORE_CLOCK Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
system_core_clock = CORE_CLOCK $ ext "SystemCoreClock"

inclSystem:: ModuleDef
inclSystem = do
    inclSym system_core_clock