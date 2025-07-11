{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Transport.UDP.RBUS.Rx    where

import           Control.Monad           (void)
import           Core.Actions
import           Data.Queue
import           Data.Serialize
import           Interface.ENET
import           Interface.LwipPort
import           Ivory.Language
import           Ivory.Stdlib
import           Support.Lwip.IP_addr
import           Support.Lwip.Netif
import           Support.Lwip.Pbuf
import           Support.Lwip.Udp
import           Transport.UDP.RBUS.Data
import           Transport.UDP.RBUS.Tx



rxTask :: (LwipPort e, Enet e) => e -> RBUS ->  Ivory (ProcEffects s ()) ()
rxTask enet RBUS{..} = do
    reval <- rxFrameSize enet
    when (reval >? 1) $
        void $ inputLwipPortIf enet netif


receiveCallback :: RBUS -> Def (UdpRecvFn s1 s2 s3 s4)
receiveCallback rbus@RBUS{..} = proc "udp_echo_callback" $ \_ upcb pbuff addr port -> body $ do
    size <- castDefault <$> deref (pbuff ~> tot_len)
    when (size >? 0 .&& size <=? arrayLen rxBuff) $ do
        for (toIx size) $ \ix ->
            store (rxBuff ! ix) =<< getPbufAt pbuff (castDefault $ fromIx ix)
        receive rbus size
    ret =<< freePbuf pbuff



receive :: RBUS -> Uint8 -> Ivory (ProcEffects s t) ()
receive rbus@RBUS{..} len = do
    action <- deref $ rxBuff ! 0
    cond_ [ action ==? actionDiscovery ==> handleDiscovery rbus len
          , action ==? actionIpAddress ==> handleAddress   rbus len
          , true                       ==> handleMessage   rbus len
          ]


handleDiscovery :: RBUS -> Uint8 -> Ivory (ProcEffects s t) ()
handleDiscovery rbus@RBUS{..} size =
    when (size ==? 7) $ do
        ip1 <- unpack rxBuff 1
        ip2 <- unpack rxBuff 2
        ip3 <- unpack rxBuff 3
        ip4 <- unpack rxBuff 4
        createIpAddr4 serverIP ip1 ip2 ip3 ip4
        store serverPort =<< unpackBE rxBuff 5
        store shouldDiscovery true



handleAddress :: RBUS -> Uint8 -> Ivory (ProcEffects s t) ()
handleAddress rbus@RBUS{..} size =
    when (size ==? 15) $ do
        isValid <- local $ ival true
        arrayMap $ \ix -> do
            m  <- deref $ mac ! ix
            m' <- deref $ rxBuff ! toIx (1 + fromIx ix)
            when (m /=? m') $ do
                store isValid false
                breakOut
        isValid' <- deref isValid
        when isValid' $ do
            ip1 <- unpack rxBuff  7
            ip2 <- unpack rxBuff  8
            ip3 <- unpack rxBuff  9
            ip4 <- unpack rxBuff 10
            createIpAddr4 localIP ip1 ip2 ip3 ip4
            nm1 <- unpack rxBuff 11
            nm2 <- unpack rxBuff 12
            nm3 <- unpack rxBuff 13
            nm4 <- unpack rxBuff 14
            createIpAddr4 netmask nm1 nm2 nm3 nm4
            setNetifAddr netif localIP netmask ipAddrAny
            store hasIP true


handleMessage :: RBUS -> Uint8 -> Ivory (ProcEffects s t) ()
handleMessage RBUS{..} = onMessage rxBuff
