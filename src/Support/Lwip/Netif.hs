{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Support.Lwip.Netif
    ( NETIF_FLAG
    , netif_flag_up

    , NETIF_STRUCT
    , NETIF

    , NetifInitFn
    , PtrNetifInitFn

    , NetifInputFn
    , PtrNetifInputFn

    , addNetif
    , setNetifDefault
    , setUpNetif
    , setNetifStatusCallback

    ,
    ) where

import           Ivory.Language
import           Ivory.Language.Proc   (ProcType, ProcPtr)
import           Ivory.Language.Syntax (Sym)
import           Ivory.Support
import           Support.Lwip.IP_addr
import           Support.Lwip.Pbuf
import           Support.Lwip.Err

headerFile :: HeaderFile
headerFile = "netif.h"

fun :: ProcType f => Sym -> Def f
fun = funFrom headerFile

ext :: IvoryExpr e => Sym -> e
ext  = extFrom headerFile


newtype NETIF_FLAG = NETIF_FLAG Uint8
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

netif_flag_up = NETIF_FLAG $ ext "NETIF_FLAG_UP"



type NETIF_STRUCT = "netif"
type NETIF s = Ref s (Struct NETIF_STRUCT)

[ivory|
    abstract struct netif "netif.h"
|]


type NET_MASK s = IP_ADDR_4 s
type GW s = IP_ADDR_4 s

type NetifInitFn s = '[NETIF s] :-> ErrT
type PtrNetifInitFn s =  Def ('[ProcPtr (NetifInitFn s)] :-> ())

type NetifInputFn s1 s2 = '[PBUF s1, NETIF s2] :-> ErrT
type PtrNetifInputFn s1 s2 =  Def ('[ProcPtr (NetifInputFn s1 s2)] :-> ())

type NetifStatusCallbackFn s = '[NETIF s] :-> ()
type PtrNetifStatusCallbackFn s = Def ('[ProcPtr (NetifStatusCallbackFn s)] :-> ())



addNetif :: NETIF s -> IP_ADDR_4 s -> NET_MASK s -> GW s -> ProcPtr ('[] :-> ()) -> PtrNetifInitFn s -> PtrNetifInputFn s1 s2 -> Ivory eff ErrT
addNetif = call netif_add

netif_add :: Def ('[NETIF s, IP_ADDR_4 s, NET_MASK s, GW s, ProcPtr ('[] :-> ()), PtrNetifInitFn s, PtrNetifInputFn s1 s2] :-> ErrT)
netif_add = fun "netif_add"


setNetifDefault :: NETIF s -> Ivory eff ()
setNetifDefault = call_ netif_set_default

netif_set_default :: Def ('[NETIF s] :-> ())
netif_set_default = fun "netif_set_default"


setUpNetif :: NETIF s -> Ivory eff ()
setUpNetif = call_ netif_set_up

netif_set_up :: Def ('[NETIF s] :-> ())
netif_set_up = fun "netif_set_up"


setNetifStatusCallback :: NETIF s -> PtrNetifStatusCallbackFn s -> Ivory eff ()
setNetifStatusCallback = call_ netif_set_status_callback

netif_set_status_callback :: Def ('[NETIF s, PtrNetifStatusCallbackFn s] :-> ())
netif_set_status_callback = fun "netif_set_status_callback"


inclNetif :: ModuleDef
inclNetif = do
    incl netif_add
    incl netif_set_default
    incl netif_set_up
    incl netif_set_status_callback
    
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
