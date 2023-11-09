{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Support.Lwip.Mem
    ( initMem

    , inclMem
    ) where

import           Ivory.Language
import           Ivory.Support
import           Ivory.Language.Proc
import           Ivory.Language.Syntax



fun :: ProcType f => Sym -> Def f
fun = funFrom "mem.h"



initMem ::  Ivory eff ()
initMem = call_ mem_init

mem_init :: Def ('[] :-> ())
mem_init = fun "mem_init"



inclMem :: ModuleDef
inclMem = do
    incl mem_init
