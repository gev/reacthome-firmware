{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE FlexibleInstances #-}

module Support.Lwip.Netif
    ( NETIF_STRUCT
    , NETIF

    , addNetif
    , setNetifDefault
    , setUpNetif

    ,
    ) where

import           Ivory.Language
import           Ivory.Language.Proc   (ProcType)
import           Ivory.Language.Syntax (Sym)
import           Ivory.Support
import           Support.Lwip.IP_addr
import           Support.Lwip.Pbuf
import           Support.Lwip.Err


fun :: ProcType f => Sym -> Def f
fun = funFrom "netif.h"


type NETIF_STRUCT = "netif"
type NETIF s = Ref s (Struct NETIF_STRUCT)


[ivory|
    abstract struct netif "netif.h"
|]


type IP_ADDR = IP_ADDR_4
type NET_MASK = IP_ADDR_4
type GW = IP_ADDR_4


type NetifInitFn s = '[NETIF s] :-> ErrT;
type NetifInputFn s = '[PBUF s, NETIF s] :-> ErrT;

addNetif :: NETIF s -> IP_ADDR -> NET_MASK -> GW -> NetifInitFn s -> NetifInputFn -> Ivory eff () -- after GW  "state = NULL"
addNetif = call_ netif_add

netif_add :: Def ('[NETIF s, IP_ADDR, NET_MASK, GW, NetifInitFn s, NetifInputFn] :-> ErrT)
netif_add = fun "netif_add"


setNetifDefault :: NETIF s -> Ivory eff()
setNetifDefault = call_ netif_set_default

netif_set_default :: Def ('[NETIF s] :-> ())
netif_set_default = fun "netif_set_default"


setUpNetif :: NETIF s -> Ivory eff()
setUpNetif = call_ netif_set_up

netif_set_up :: Def ('[NETIF] :-> ())
netif_set_up = fun "netif_set_up"


inclNetif :: ModuleDef
inclNetif = do
    incl netif_add
    incl netif_set_default
    incl netif_set_up




-- type NetifInitFn s = '[NETIF s] :-> ErrT;

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
