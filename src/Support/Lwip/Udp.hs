{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Support.Lwip.Udp (
    UdpRecvFn,
    PtrUdpRecvFn,
    UDP_PCB_STRUCT,
    UDP_PCB,
    newUdp,
    removeUdp,
    bindUdp,
    sendUdp,
    connectUdp,
    disconnectUdp,
    recvUdp,
    inclUdp,
) where

import Ivory.Language
import Ivory.Language.Proc (ProcType)
import Ivory.Language.Syntax (Sym)
import Ivory.Support
import Support.Lwip.Err
import Support.Lwip.IP_addr
import Support.Lwip.Pbuf

fun :: (ProcType f) => Sym -> Def f
fun = funFrom "lwip/udp.h"

type UDP_PCB_STRUCT = "udp_pcb"
type UDP_PCB s = Ptr s (Struct UDP_PCB_STRUCT)

[ivory|
    abstract struct udp_pcb "udp.h"
|]

type UdpRecvFn s1 s2 s3 s4 = '[Ptr s1 (Stored ()), UDP_PCB s2, PBUF s3, IP_ADDR_4 s4, Uint16] :-> ErrT
type PtrUdpRecvFn s1 s2 s3 s4 = ProcPtr (UdpRecvFn s1 s2 s3 s4)

newUdp :: Ivory eff (UDP_PCB s)
newUdp = call udp_new

udp_new :: Def ('[] :-> UDP_PCB s)
udp_new = fun "udp_new"

removeUdp :: UDP_PCB s1 -> Ivory eff ()
removeUdp = call_ udp_remove

udp_remove :: Def ('[UDP_PCB s] :-> ())
udp_remove = fun "udp_remove"

bindUdp :: UDP_PCB s1 -> IP_ADDR_4 s2 -> Uint16 -> Ivory eff ErrT
bindUdp = call udp_bind

udp_bind :: Def ('[UDP_PCB s1, IP_ADDR_4 s2, Uint16] :-> ErrT)
udp_bind = fun "udp_bind"

connectUdp :: UDP_PCB s1 -> IP_ADDR_4 s2 -> Uint16 -> Ivory eff ErrT
connectUdp = call udp_connect

udp_connect :: Def ('[UDP_PCB s1, IP_ADDR_4 s2, Uint16] :-> ErrT)
udp_connect = fun "udp_connect"

disconnectUdp :: UDP_PCB s -> Ivory eff ()
disconnectUdp = call_ udp_disconnect

udp_disconnect :: Def ('[UDP_PCB s] :-> ())
udp_disconnect = fun "udp_disconnect"

sendUdp :: UDP_PCB s1 -> PBUF s2 -> Ivory eff ErrT
sendUdp = call udp_send

udp_send :: Def ('[UDP_PCB s1, PBUF s2] :-> ErrT)
udp_send = fun "udp_send"

recvUdp :: UDP_PCB s1 -> PtrUdpRecvFn s2 s3 s4 s5 -> Ptr s6 (Stored ()) -> Ivory eff ()
recvUdp = call_ udp_recv

udp_recv :: Def ('[UDP_PCB s1, PtrUdpRecvFn s2 s3 s4 s5, Ptr s6 (Stored ())] :-> ())
udp_recv = fun "udp_recv"

inclUdp :: ModuleDef
inclUdp = do
    incl udp_new
    incl udp_remove
    incl udp_bind
    incl udp_send
    incl udp_connect
    incl udp_disconnect
    incl udp_recv
