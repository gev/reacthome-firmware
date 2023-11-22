{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Support.Lwip.Memp
    ( initMemp

    , inclMemp
    ) where

import           Ivory.Language
import           Ivory.Support
import           Ivory.Language.Proc
import           Ivory.Language.Syntax



fun :: ProcType f => Sym -> Def f
fun = funFrom "lwip/memp.h"



initMemp ::  Ivory eff ()
initMemp = call_ memp_init

memp_init :: Def ('[] :-> ())
memp_init = fun "memp_init"



inclMemp :: ModuleDef
inclMemp = do
    incl memp_init
