{-# HLINT ignore "Use camelCase" #-}

module Support.Lwip.Mem (
    initMem,
    inclMem,
) where

import Ivory.Language
import Ivory.Language.Proc
import Ivory.Language.Syntax
import Ivory.Support

fun :: (ProcType f) => Sym -> Def f
fun = funFrom "lwip/mem.h"

initMem :: Ivory eff ()
initMem = call_ mem_init

mem_init :: Def ('[] :-> ())
mem_init = fun "mem_init"

inclMem :: ModuleDef
inclMem = do
    incl mem_init
