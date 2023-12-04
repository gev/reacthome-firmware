{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Transport.UDP.RBUS where

import           Control.Monad                                     (void)
import           Control.Monad.Reader                              (MonadReader,
                                                                    asks)
import           Control.Monad.State                               (MonadState)
import           Core.Context
import           Core.Controller
import           Core.Dispatcher
import           Core.Domain                                       (Domain (version))
import qualified Core.Domain                                       as D
import           Core.Feature
import           Core.Handler
import           Core.Task
import           Core.Transport
import           Core.Version
import           Data.Buffer
import           Data.Record
import           Data.Serialize
import           Data.Value
import           Device.GD32F4xx
import           Feature.Server                                    (server)
import           GHC.Arr                                           (array)
import           GHC.TypeNats
import           Interface.ENET
import           Interface.LwipPort
import           Interface.MCU                                     as I
import           Interface.SystemClock                             (getSystemTime)
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Stdlib.Control
import           Support.Device.GD32F4xx.LwipPort.Basic.Ethernetif
import           Support.Lwip.Etharp
import           Support.Lwip.Ethernet
import           Support.Lwip.Igmp
import           Support.Lwip.IP_addr
import           Support.Lwip.Mem
import           Support.Lwip.Memp
import           Support.Lwip.Netif
import           Support.Lwip.Pbuf
import           Support.Lwip.Udp
import           Transport.UDP.RBUS.Data
import           Util.String                                       (memCmp)



rbus :: (MonadState Context m, MonadReader (D.Domain p t) m, Enet e, LwipPort e)
      => (p -> m e) -> m RBUS
rbus enet' = do
    mcu         <- asks D.mcu
    model       <- asks D.model
    version     <- asks D.version
    let mac      = I.mac mcu
    features    <- asks D.features
    enet        <- enet' $ peripherals mcu
    netif       <- record_ "udp_netif"
    upcb        <- value_  "udp_upcb"
    serverIP    <- record_ "udp_server_ip"
    serverPort  <- value   "udp_server_port" 2017
    localIP     <- record_ "udp_local_ip"
    netmask     <- record_ "udp_netmask"
    broadcastIP <- record_ "udp_broadcast_ip"
    hasIP       <- value   "udp_has_ip" false
    rxBuff      <- buffer  "udp_rx"
    txBuff      <- buffer  "udp_tx"
    discovery   <- buffer  "udp_discovery"
    requestIP   <- buffer  "udp_request_ip"
    requestInit <- buffer  "udp_request_init"

    {--
        TODO: move dispatcher outside
    --}
    let onMessage = makeDispatcher features

    let rbus   = RBUS { mac
                      , netif
                      , upcb
                      , serverIP, serverPort
                      , localIP, netmask
                      , broadcastIP
                      , hasIP
                      , txBuff
                      , rxBuff
                      , discovery
                      , requestIP
                      , requestInit
                      , onMessage
                      }

    addModule inclEthernet
    addModule inclEthernetif
    addModule inclNetif
    addModule inclUdp
    addModule inclMem
    addModule inclMemp
    addModule inclIP_addr
    addModule inclPbuf
    addModule inclEtharp
    addModule inclIgmp

    let sysNow :: Def ('[] :-> Uint32)
        sysNow = proc "sys_now" $ body $
            ret =<< getSystemTime (systemClock mcu)

    addProc sysNow
    addProc $ netifStatusCallback rbus
    addProc $ receiveCallback rbus

    addInit "udp_init" $ do
        initMem
        initMemp

        createIpAddr4 serverIP    255 255 255 255
        createIpAddr4 localIP     169 254  47  94
        createIpAddr4 netmask     255 255   0   0
        createIpAddr4 broadcastIP 255 255 255 255

        store (netif ~> hwaddr_len) 6

        arrayMap $ \ix -> do
            m <- deref (mac ! ix)
            store (netif ~> hwaddr ! ix) m
            store (txBuff ! toIx ix) m

        store (discovery   ! 0) 0xf0
        store (discovery   ! 1) =<< deref model
        store (discovery   ! 2) =<< deref (version ~> major)
        store (discovery   ! 3) =<< deref (version ~> minor)

        store (requestIP   ! 0) 0xfd

        store (requestInit ! 0) 0xf2

        addNetif netif localIP netmask ipAddrAny nullPtr (initLwipPortIf enet) inputEthernetPtr
        setNetifDefault netif
        setNetifStatusCallback netif $ procPtr $ netifStatusCallback rbus

        initIgmp

        startIgmp netif
        setUpNetif netif

    addHandler $ HandleEnet enet $ do
        reval <- rxFrameSize enet
        when (reval >? 1) $
            void $ inputLwipPortIf enet netif

    addTask $ delay 1000 "tmr_arp"  tmrEtharp
    addTask $ delay  100 "tmr_igmp" tmrIgmp

    pure rbus



netifStatusCallback :: RBUS -> Def (NetifStatusCallbackFn s)
netifStatusCallback rbus@RBUS{..} = proc "netif_callback" $ \netif -> body $ do
     flags' <- deref $ netif ~> flags
     when (flags' .& netif_flag_up /=?  0) $ do
        upcb' <- newUdp
        store upcb upcb'
        when (upcb' /=? nullPtr) $ do
          err <- bindUdp upcb' ipAddrAny 2017
          when (err ==? 0) $ do
            recvUdp upcb' (procPtr $ receiveCallback rbus) nullPtr



receiveCallback :: RBUS -> Def (UdpRecvFn s1 s2 s3 s4)
receiveCallback rbus@RBUS{..} = proc "udp_echo_callback" $ \_ upcb pbuff addr port -> body $ do
    len <- castDefault <$> deref (pbuff ~> tot_len)
    when (len >? 0 .&& len <=? 255) $ do
        for (toIx len) $ \ix ->
            store (rxBuff ! ix) =<< getPbufAt pbuff (castDefault $ fromIx ix)
        receive rbus len
    ret =<< freePbuf pbuff



receive :: RBUS -> Uint8 -> Ivory (ProcEffects s t) ()
receive rbus@RBUS{..} len = do
    action <- deref $ rxBuff ! 0
    cond_ [ action ==? 0xf0 ==> handleDiscovery rbus len
          , action ==? 0xfd ==> handleAddress   rbus len
          , true            ==> handleMessage   rbus len
          ]



handleDiscovery :: RBUS -> Uint8 -> Ivory (ProcEffects s t) ()
handleDiscovery rbus@RBUS{..} len =
    when (len ==? 7) $ do
        hasIP' <- deref hasIP
        ip1 <- unpack rxBuff 1
        ip2 <- unpack rxBuff 2
        ip3 <- unpack rxBuff 3
        ip4 <- unpack rxBuff 4
        createIpAddr4 serverIP ip1 ip2 ip3 ip4
        store serverPort =<< unpackBE rxBuff 5
        ifte_ hasIP'
            (transmit  rbus discovery)
            (broadcast rbus requestIP)



handleAddress :: RBUS -> Uint8 -> Ivory (ProcEffects s t) ()
handleAddress rbus@RBUS{..} len =
    when (len ==? 15) $ do
        isValid <- local $ ival true
        for 6 $ \ix -> do
            isValid' <- deref isValid
            m  <- deref $ mac ! ix
            m' <- deref $ rxBuff ! toIx (1 + fromIx ix)
            when (m ==? m') $
                store isValid (isValid' .&& m ==? m')
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
            transmit rbus requestInit


handleMessage :: RBUS -> Uint8 -> Ivory (ProcEffects s t) ()
handleMessage RBUS{..} = onMessage rxBuff



instance Transport RBUS where
  transmitBuffer = transmit

instance LazyTransport RBUS where
  lazyTransmit = lazyTransmit'



transmit :: KnownNat n => RBUS -> Buffer n Uint8 -> Ivory (ProcEffects s t) ()
transmit rbus@RBUS{..} = transmit' serverIP rbus


broadcast :: KnownNat n => RBUS -> Buffer n Uint8 -> Ivory (ProcEffects s t) ()
broadcast rbus@RBUS{..} = transmit' broadcastIP rbus



transmit' :: KnownNat n => IP_ADDR_4 s1 -> RBUS -> Buffer n Uint8 -> Ivory (ProcEffects s2 t) ()
transmit' ip RBUS{..} buff = do
    upcb' <- deref upcb
    err <- connectUdp upcb' ip =<< deref serverPort
    when (err ==? 0) $ do
        -- arrayMap $ \ix -> store (txBuff ! toIx (fromIx ix)) =<< deref (mac ! ix)
        arrayMap $ \ix -> store (txBuff ! toIx (6 + fromIx ix)) =<< deref (buff ! ix)
        pbuff <- allocPbufReference (toCArray txBuff) (6 + arrayLen buff) pbuf_ref
        sendUdp upcb' pbuff
        disconnectUdp upcb'
        void $ freePbuf pbuff



lazyTransmit' :: RBUS -> ((Uint8 -> forall eff. Ivory eff ())
                                 -> forall eff. Ivory eff ())
                      -> Ivory (ProcEffects s t) ()
lazyTransmit' RBUS{..} transmit = do
    upcb' <- deref upcb
    err <- connectUdp upcb' serverIP =<< deref serverPort
    when (err ==? 0) $ do
        -- arrayMap $ \ix -> store (txBuff ! toIx (fromIx ix)) =<< deref (mac ! ix)
        len   <- run transmit txBuff
        pbuff <- allocPbufReference (toCArray txBuff) len pbuf_ref
        sendUdp upcb' pbuff
        disconnectUdp upcb'
        void $ freePbuf pbuff


run :: KnownNat n
    => ((Uint8 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s t) ())
    -> Buffer n Uint8
    -> Ivory (ProcEffects s t) Uint16
run transmit buff = do
    len <- local $ ival 6
    let go :: Uint8 -> Ivory eff ()
        go v = do
            i <- deref len
            let ix = toIx i
            store (buff ! ix) v
            store len $ i + 1
    transmit go
    deref len





-- │[ 0] daemon                     Mem: 107 MB    CPU:  0 %  online ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 01 04 00>                                                                                          │
-- │[ 1] rbus                      Mem:   0 MB    CPU:  0 %  stopped ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 a1 b0 fb 7d df 75 13 01 00 f0 2b 02 02>                                                            │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 a1 b0 fb 7d df 75 13 01 00 f2>                                                                     │
-- │                                                                 ││ daemon > send  224.0.0.1 <Buffer f0 ac 10 00 01 07 e0>                                                                                                    │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 f0 c0 03 00>                                                                                       │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 01 04 00>                                                                                          │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 01 03 00>                                                                                          │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 a1 b0 fb 7d df 75 13 01 00 f0 2b 02 02>                                                            │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 a1 b0 fb 7d df 75 13 01 00 f2>                                                                     │
-- │                                                                 ││ daemon > send  224.0.0.1 <Buffer f0 ac 10 00 01 07 e0>                                                                                                    │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 f0 c0 03 00>                                                                                       │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 01 03 00>                                                                                          │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 01 04 00>                                                                                          │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 a1 b0 fb 7d df 75 13 01 00 f0 2b 02 02>                                                            │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 a1 b0 fb 7d df 75 13 01 00 f2>                                                                     │
-- │                                                                 ││ daemon > send  224.0.0.1 <Buffer f0 ac 10 00 01 07 e0>                                                                                                    │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 f0 c0 03 00>                                                                                       │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 01 03 00>                                                                                          │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 01 04 00>                                                                                          │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 a1 b0 fb 7d df 75 13 01 00 f0 2b 02 02>                                                            │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 a1 b0 fb 7d df 75 13 01 00 f2>                                                                     │
-- │                                                                 ││ daemon > send  224.0.0.1 <Buffer f0 ac 10 00 01 07 e0>                                                                                                    │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 f0 c0 03 00>                                                                                       │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 01 04 00>                                                                                          │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 01 03 00>                                                                                          │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 a1 b0 fb 7d df 75 13 01 00 c5 8f 03>                                                               │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 a1 b0 fb 7d df 75 13 01 00 c0 38 0b>                                                               │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 a1 b0 fb 7d df 75 13 01 00 c2 ab 07>                                                               │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 a1 b0 fb 7d df 75 13 01 00 f0 2b 02 02>                                                            │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 a1 b0 fb 7d df 75 13 01 00 f2>                                                                     │
-- │                                                                 ││ daemon > send  224.0.0.1 <Buffer f0 ac 10 00 01 07 e0>                                                                                                    │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 f0 c0 03 00>                                                                                       │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 01 04 00>                                                                                          │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 01 03 00>                                                                                          │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 a1 b0 fb 7d df 75 13 01 00 f0 2b 02 02>                                                            │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 a1 b0 fb 7d df 75 13 01 00 f2>                                                                     │
-- │                                                                 ││ daemon > send  224.0.0.1 <Buffer f0 ac 10 00 01 07 e0>                                                                                                    │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 f0 c0 03 00>                                                                                       │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 01 03 00>                                                                                          │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 01 04 00>                                                                                          │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 a1 b0 fb 7d df 75 13 01 00 f0 2b 02 02>                                                            │
-- │                                                                 ││ daemon > receive  172.16.0.7 <Buffer 6c bc 71 1f 35 30 a1 b0 fb 7d df 75 13 01 00 f2>
