{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE FlexibleInstances #-}

module Support.Lwip.Pbuf
    ( PBUF_STRUCT
    , PBUF
    , freePbuf

    , inclPbuf
    ) where

import           Ivory.Language
import           Ivory.Language.Proc   (ProcType)
import           Ivory.Language.Syntax (Sym)
import           Ivory.Support
import           Support.Lwip.Err



fun :: ProcType f => Sym -> Def f
fun = funFrom "pbuf.h"


type PBUF_STRUCT = "pbuf"
type PBUF s = Ref s (Struct PBUF_STRUCT)


[ivory|
    abstract struct pbuf "pbuf.h"
|]


freePbuf :: PBUF s -> Ivory eff ErrT
freePbuf = call pbuf_free

pbuf_free :: Def ('[PBUF s] :-> ErrT)
pbuf_free = fun "pbuf_free"


inclPbuf :: ModuleDef
inclPbuf = do
    incl pbuf_free