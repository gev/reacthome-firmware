{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Support.Lwip.Udp
    ( UdpRecvFn
    , PtrUdpRecvFn
    
    , newUdp
    , bindUdp
    , sendUdp
    , connectUdp
    , disconnectUdp
    , recvUdp

    , inclUdp
    ) where

import           Ivory.Language
import           Ivory.Language.Proc   (ProcType)
import           Ivory.Language.Syntax (Sym)
import           Ivory.Support
import           Support.Lwip.IP_addr
import           Support.Lwip.Pbuf
import           Support.Lwip.Err


fun :: ProcType f => Sym -> Def f
fun = funFrom "udp.h"



type UDP_PCB_STRUCT = "udp_pcb"
type UDP_PCB s = Ref s (Struct UDP_PCB_STRUCT)

[ivory|
    abstract struct udp_pcb "udp.h"
|]


type UdpRecvFn s1 s2 s3  = '[ProcPtr ('[] :-> ()), UDP_PCB s1, PBUF s2, IP_ADDR_4 s3, Uint16] :-> ErrT
type PtrUdpRecvFn s1 s2 s3 =  Def ('[ProcPtr (UdpRecvFn s1 s2 s3)] :-> ())


newUdp :: Ivory eff (UDP_PCB s)
newUdp = call udp_new

udp_new :: Def ('[] :-> UDP_PCB s)
udp_new = fun "udp_new"


bindUdp :: UDP_PCB s -> IP_ADDR_4 s -> Uint16 -> Ivory eff ErrT
bindUdp = call udp_bind

udp_bind :: Def ('[UDP_PCB s, IP_ADDR_4 s, Uint16] :-> ErrT)
udp_bind = fun "udp_bind"


connectUdp :: UDP_PCB s -> IP_ADDR_4 s -> Uint16 -> Ivory eff ErrT
connectUdp = call udp_connect

udp_connect :: Def ('[UDP_PCB s, IP_ADDR_4 s, Uint16] :-> ErrT)
udp_connect = fun "udp_connect"

 
disconnectUdp :: UDP_PCB s -> Ivory eff ()
disconnectUdp = call_ udp_disconnect

udp_disconnect :: Def ('[UDP_PCB s] :-> ())
udp_disconnect = fun "udp_disconnect"


sendUdp :: UDP_PCB s -> PBUF s2 -> Ivory eff ErrT
sendUdp = call udp_send

udp_send :: Def ('[UDP_PCB s, PBUF s2] -> ErrT)
udp_send = fun "udp_send"


recvUdp :: UDP_PCB s -> PtrUdpRecvFn s1 s2 s3 -> ProcPtr ('[] :-> ()) -> Ivory eff ()
recvUdp = call_ udp_recv

udp_recv :: Def ('[UDP_PCB s, PtrUdpRecvFn, ProcPtr ('[] :-> ())] :-> ())
udp_recv = fun "udp_recv"


inclUdp :: ModuleDef
inclUdp = do
    incl udp_new
    incl udp_bind
    incl udp_send
    incl udp_connect
    incl udp_disconnect
    incl udp_recv

