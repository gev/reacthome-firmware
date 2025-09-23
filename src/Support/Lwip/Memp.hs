{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Support.Lwip.Memp (
    initMemp,
    inclMemp,
) where

import Ivory.Language
import Ivory.Language.Proc
import Ivory.Language.Syntax
import Ivory.Support

fun :: (ProcType f) => Sym -> Def f
fun = funFrom "lwip/memp.h"

initMemp :: Ivory eff ()
initMemp = call_ memp_init

memp_init :: Def ('[] :-> ())
memp_init = fun "memp_init"

inclMemp :: ModuleDef
inclMemp = do
    incl memp_init
