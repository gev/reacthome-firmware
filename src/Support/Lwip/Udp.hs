{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Support.Lwip.Udp
    ( newUdp

    , inclUdp
    ) where

import           Ivory.Language
import           Ivory.Language.Proc
import           Ivory.Language.Syntax
import           Ivory.Support


fun :: ProcType f => Sym -> Def f
fun = funFrom "lwip/udp.h"


type UDP_PCB_STRUCT = "netif"
type UDP_PCB s = Ref s (Struct UDP_PCB_STRUCT)


newUdp :: Ivory eff UDP_PCB
newUdp = call udp_new

udp_new :: Def ('[] :-> (UDP_PCB))
udp_new = fun "udp_new"


inclUdp :: ModuleDef
inclUdp = do
    incl udp_new
