{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Support.Lwip.Udp
    ( newUdp
    , bindUdp
    , connectUdp

    , inclUdp
    ) where

import           Ivory.Language
import           Ivory.Language.Proc   (ProcType)
import           Ivory.Language.Syntax (Sym)
import           Ivory.Support
import           Support.Lwip.IP_addr
import           Support.Lwip.Err

type ErrT = Sint8;

fun :: ProcType f => Sym -> Def f
fun = funFrom "udp.h"


type UDP_PCB_STRUCT = "udp_pcb"
type UDP_PCB s = Ref s (Struct UDP_PCB_STRUCT)

[ivory|
    abstract struct udp_pcb "udp.h"
|]


newUdp :: Ivory eff (UDP_PCB s)
newUdp = call udp_new

udp_new :: Def ('[] :-> UDP_PCB s)
udp_new = fun "udp_new"


bindUdp :: UDP_PCB s -> IP_ADDR_4 s -> Sint -> Ivory eff ErrT
bindUdp = call udp_bind

udp_bind :: Def ('[UDP_PCB s, IP_ADDR_4 s, Sint] :-> ErrT)
udp_bind = fun "udp_bind"


connectUdp :: UDP_PCB s -> IP_ADDR_4 s -> Sint -> Ivory eff ErrT
connectUdp = call udp_connect

udp_connect :: Def ('[UDP_PCB s, IP_ADDR_4 s, Sint] :-> ErrT)
udp_connect = fun "udp_connect"

 
disconnectUdp :: UDP_PCB s -> Ivory eff
disconnectUdp = call_ udp_disconnect

udp_disconnect :: Def ('[UDP_PCB s] -> ())
udp_disconnect = fun "udp_disconnect"
-- udp_recv_fn

inclUdp :: ModuleDef
inclUdp = do
    incl udp_new
    incl udp_bind
    incl udp_connect

