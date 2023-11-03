{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Support.Lwip.Netif
    ( NETIF_STRUCT
    , NETIF

    , addNetif
    , setNetifDefault
    , setUpNetif

    , 
    ) where

import           Ivory.Language
import           Ivory.Support
import           Ivory.Language.Proc
import           Ivory.Language.Syntax
import           Support.Lwip.IP_addr


fun :: ProcType f => Sym -> Def f
fun = funFrom "lwip/netif.h"


type NETIF_STRUCT = "netif"
type NETIF s = Ref s (Struct NETIF_STRUCT)


type IP_ADDR = IP_ADDR_4
type NET_MASK = IP_ADDR_4
type GW = IP_ADDR_4



addNetif :: NETIF -> IP_ADDR -> NET_MASK -> GW -> 
addNetif = call_ netif_add

netif_add :: 
netif_add = fun "netif_add"


setNetifDefault :: NETIF -> Ivory eff()
setNetifDefault = call_ netif_set_default

netif_set_default :: Def ('[NETIF] :-> ())
netif_set_default = fun "netif_set_default"


setUpNetif :: NETIF -> Ivory eff()
setUpNetif = call_ netif_set_up

netif_set_up :: Def ('[NETIF] :-> ())
netif_set_up = fun "netif_set_up"


inclNetif :: ModuleDef
inclNetif = do 
    incl netif_add
    incl netif_set_default
    incl netif_set_up


