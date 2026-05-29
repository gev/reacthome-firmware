{-# HLINT ignore "Use for_" #-}

module Transport.UDP.RBUS.Rx where

import Core.Actions
import Data.Serialize
import Ivory.Language
import Ivory.Stdlib
import Support.Lwip.IP_addr
import Support.Lwip.Pbuf
import Support.Lwip.Udp
import Transport.UDP.RBUS.Data

receiveCallback :: RBUS -> Def (UdpRecvFn s1 s2 s3 s4)
receiveCallback rbus@RBUS{..} =
    proc "udp_echo_callback" \_ _ pbuff _ _ -> body do
        size <- castDefault <$> deref (pbuff ~> tot_len)
        when (size >? 0 .&& size <=? arrayLen rxBuff) do
            for (toIx size) \ix ->
                store (rxBuff ! ix) =<< getPbufAt pbuff (castDefault $ fromIx ix)
            receive rbus size
        ret =<< freePbuf pbuff

receive :: RBUS -> Uint8 -> Ivory (ProcEffects s t) ()
receive rbus@RBUS{..} len = do
    action <- deref $ rxBuff ! 0
    cond_
        [ action ==? actionDiscovery ==> handleDiscovery rbus len
        , true ==> handleMessage rbus len
        ]

handleDiscovery :: RBUS -> Uint8 -> Ivory (ProcEffects s t) ()
handleDiscovery RBUS{..} size =
    when (size ==? 7) do
        ip1 <- unpack rxBuff 1
        ip2 <- unpack rxBuff 2
        ip3 <- unpack rxBuff 3
        ip4 <- unpack rxBuff 4
        createIpAddr4 serverIP ip1 ip2 ip3 ip4
        store serverPort =<< unpackBE rxBuff 5
        store shouldDiscovery true


handleMessage :: RBUS -> Uint8 -> Ivory (ProcEffects s t) ()
handleMessage RBUS{..} = onMessage rxBuff
