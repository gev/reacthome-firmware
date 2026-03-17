{-# HLINT ignore "Use camelCase" #-}

module Support.RunAppByAddr (
    runAppByAddr,
    inclRunAppByAddr,
) where

import Ivory.Language

runAppByAddr :: Uint32 -> Ivory eff ()
runAppByAddr = call_ run_app_by_addr

run_app_by_addr :: Def ('[Uint32] :-> ())
run_app_by_addr = importProc "run_app_by_addr" "run_app_by_addr.h"

inclRunAppByAddr :: ModuleDef
inclRunAppByAddr = incl run_app_by_addr
