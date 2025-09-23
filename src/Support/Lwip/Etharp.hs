{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Support.Lwip.Etharp (
    ARP_INTERVAL,
    arp_tmr_interval,
    tmrEtharp,
    inclEtharp,
) where

import Ivory.Language
import Ivory.Language.Proc (ProcType)
import Ivory.Language.Syntax (Sym)
import Ivory.Support

headerFile :: HeaderFile
headerFile = "lwip/etharp.h"

fun :: (ProcType f) => Sym -> Def f
fun = funFrom headerFile

ext :: (IvoryExpr e) => Sym -> e
ext = extFrom headerFile

newtype ARP_INTERVAL = ARP_INTERVAL Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

arp_tmr_interval = ARP_INTERVAL $ ext "ARP_TMR_INTERVAL"

tmrEtharp :: Ivory eff ()
tmrEtharp = call_ etharp_tmr

etharp_tmr :: Def ('[] :-> ())
etharp_tmr = fun "etharp_tmr"

inclEtharp = do
    incl etharp_tmr

    inclSym arp_tmr_interval
