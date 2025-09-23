{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Support.ReadAddr (
    readAddr32u,
    inclReadAddr,
) where

import Ivory.Language
import Ivory.Language.Module

inclReadAddr :: ModuleDef
inclReadAddr = incl read_addr_32u

readAddr32u :: Uint32 -> Ivory eff Uint32
readAddr32u = call read_addr_32u

read_addr_32u :: Def ('[Uint32] :-> Uint32)
read_addr_32u = importProc "read_addr_32u" "read_addr.h"
