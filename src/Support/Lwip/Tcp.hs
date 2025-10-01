{-# HLINT ignore "Use camelCase" #-}

module Support.Lwip.Tcp (
    TCP_INTERVAL,
    tcp_tmr_interval,
    tmrTcp,
    inclTcp,
) where

import Ivory.Language
import Ivory.Language.Proc (ProcType)
import Ivory.Language.Syntax (Sym)
import Ivory.Support

headerFile :: HeaderFile
headerFile = "lwip/tcp.h"

fun :: (ProcType f) => Sym -> Def f
fun = funFrom headerFile

ext :: (IvoryExpr e) => Sym -> e
ext = extFrom headerFile

newtype TCP_INTERVAL = TCP_INTERVAL Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

tcp_tmr_interval = TCP_INTERVAL $ ext "TCP_TMR_INTERVAL"

tmrTcp :: Ivory eff ()
tmrTcp = call_ tcp_tmr

tcp_tmr :: Def ('[] :-> ())
tcp_tmr = fun "tcp_tmr"

inclTcp = do
    incl tcp_tmr

    inclSym tcp_tmr_interval
