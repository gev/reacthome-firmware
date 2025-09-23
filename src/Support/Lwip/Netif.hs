{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Support.Lwip.Netif (
    NETIF_FLAG,
    netif_flag_up,
    NETIF_STRUCT,
    NETIF,
    flags,
    hwaddr,
    hwaddr_len,
    NetifInitFn,
    PtrNetifInitFn,
    NetifInputFn,
    PtrNetifInputFn,
    NetifStatusCallbackFn,
    PtrNetifStatusCallbackFn,
    addNetif,
    setNetifDefault,
    setUpNetif,
    setNetifStatusCallback,
    setNetifAddr,
    inclNetif,
) where

import Ivory.Language
import Ivory.Language.Pointer
import Ivory.Language.Proc (ProcPtr, ProcType)
import Ivory.Language.Syntax (Sym)
import Ivory.Support
import Support.Lwip.Err
import Support.Lwip.IP_addr
import Support.Lwip.Pbuf

headerFile :: HeaderFile
headerFile = "lwip/netif.h"

fun :: (ProcType f) => Sym -> Def f
fun = funFrom headerFile

ext :: (IvoryExpr e) => Sym -> e
ext = extFrom headerFile

newtype NETIF_FLAG = NETIF_FLAG Uint8
    deriving
        ( IvoryExpr
        , IvoryInit
        , IvoryStore
        , IvoryType
        , IvoryVar
        , IvoryEq
        , IvoryBits
        , Num
        )

netif_flag_up = NETIF_FLAG $ ext "NETIF_FLAG_UP"

type NETIF_STRUCT = "netif"
type NETIF s = Ref s (Struct NETIF_STRUCT)

[ivory|
    struct netif
        { flags :: Stored NETIF_FLAG
        ; hwaddr :: Array 6 (Stored Uint8)
        ; hwaddr_len :: Stored Uint8
        }
|]

type NET_MASK s = IP_ADDR_4 s
type GW s = IP_ADDR_4 s

type NetifInitFn s = '[NETIF s] :-> ErrT
type PtrNetifInitFn s = ProcPtr (NetifInitFn s)

type NetifInputFn s1 s2 = '[PBUF s1, NETIF s2] :-> ErrT
type PtrNetifInputFn s1 s2 = ProcPtr (NetifInputFn s1 s2)

type NetifStatusCallbackFn s = '[NETIF s] :-> ()
type PtrNetifStatusCallbackFn s = ProcPtr (NetifStatusCallbackFn s)

addNetif :: NETIF s1 -> IP_ADDR_4 s2 -> NET_MASK s3 -> GW s4 -> Ptr s5 (Stored ()) -> PtrNetifInitFn s6 -> PtrNetifInputFn s7 s8 -> Ivory eff ErrT
addNetif = call netif_add

netif_add :: Def ('[NETIF s1, IP_ADDR_4 s2, NET_MASK s3, GW s4, Ptr s5 (Stored ()), PtrNetifInitFn s6, PtrNetifInputFn s7 s8] :-> ErrT)
netif_add = fun "netif_add"

setNetifDefault :: NETIF s -> Ivory eff ()
setNetifDefault = call_ netif_set_default

netif_set_default :: Def ('[NETIF s] :-> ())
netif_set_default = fun "netif_set_default"

setUpNetif :: NETIF s -> Ivory eff ()
setUpNetif = call_ netif_set_up

netif_set_up :: Def ('[NETIF s] :-> ())
netif_set_up = fun "netif_set_up"

setNetifStatusCallback :: NETIF s1 -> PtrNetifStatusCallbackFn s2 -> Ivory eff ()
setNetifStatusCallback = call_ netif_set_status_callback

netif_set_status_callback :: Def ('[NETIF s1, PtrNetifStatusCallbackFn s2] :-> ())
netif_set_status_callback = fun "netif_set_status_callback"

setNetifAddr :: NETIF s1 -> IP_ADDR_4 s2 -> NET_MASK s3 -> GW s4 -> Ivory eff ()
setNetifAddr = call_ netif_set_addr

netif_set_addr :: Def ('[NETIF s1, IP_ADDR_4 s2, NET_MASK s3, GW s4] :-> ())
netif_set_addr = fun "netif_set_addr"

inclNetif :: ModuleDef
inclNetif = do
    incl netif_add
    incl netif_set_default
    incl netif_set_up
    incl netif_set_status_callback
    incl netif_set_addr

    inclSym netif_flag_up

-- type NetifInitFn s = '[NETIF s] :-> ErrT

-- init' :: Def (NetifInitFn s)
-- init' = undefined

-- type Uint8'to'void ='[Uint8] :-> ()

-- foo :: Def Uint8'to'void
-- foo = undefined

-- bar :: Def ('[ProcPtr (NetifInitFn s)] :-> ())
-- bar = undefined

-- run :: Ivory eff ()
-- run = x
--     where
--         x = call_ bar f
--         f = procPtr init'
