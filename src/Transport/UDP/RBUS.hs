{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Transport.UDP.RBUS where

import           Control.Monad           (void)
import           Control.Monad.Reader    (MonadReader, asks)
import           Control.Monad.State     (MonadState)
import           Core.Context
import           Core.Controller
import           Core.Dispatcher
import qualified Core.Domain             as D
import           Core.Feature
import           Core.Handler
import           Core.Task
import           Core.Transport
import           Core.Version
import           Data.Buffer
import           Data.Record
import           Data.Value
import           Feature.Server          (server)
import           Interface.ENET
import           Interface.LwipPort
import           Interface.MCU           as I
import           Interface.SystemClock   (getSystemTime)
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Stdlib.Control
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
import           Transport.UDP.RBUS.Rx
import           Transport.UDP.RBUS.Tx



rbus :: (MonadState Context m, MonadReader (D.Domain p t) m, Enet e, LwipPort e)
      => (p -> m e) -> m RBUS
rbus enet' = do
    mcu             <- asks D.mcu
    model           <- asks D.model
    version         <- asks D.version
    let mac          = I.mac mcu
    features        <- asks D.features
    enet            <- enet' $ peripherals mcu
    netif           <- record_ "udp_netif"
    upcb            <- value_  "udp_upcb"
    serverIP        <- record_ "udp_server_ip"
    serverPort      <- value   "udp_server_port" 2017
    localIP         <- record_ "udp_local_ip"
    netmask         <- record_ "udp_netmask"
    broadcastIP     <- record_ "udp_broadcast_ip"
    hasIP           <- value   "udp_has_ip" false
    rxBuff          <- buffer  "udp_rx"
    txBuff          <- buffer  "udp_tx"
    discovery       <- buffer  "udp_discovery"
    requestIP       <- buffer  "udp_request_ip"
    requestInit     <- buffer  "udp_request_init"
    shouldDiscovery <- value "udp_should_discovery" false

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
                      , shouldDiscovery
                      , onMessage
                      }

    addModule inclEthernet
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
            -- store (txBuff ! toIx ix) m

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
    addTask $ yeld  "udp_discovery" $ discoveryTask rbus

    pure rbus



discoveryTask :: RBUS -> Ivory (ProcEffects s t) ()
discoveryTask rbus@RBUS{..} = do
    shouldDiscovery' <- deref shouldDiscovery
    when shouldDiscovery' $ do
        hasIP' <- deref hasIP
        ifte_ hasIP'
            (transmit  rbus discovery)
            (broadcast rbus requestIP)
        store shouldDiscovery false



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


instance Transport RBUS where
  transmitBuffer = transmit

instance LazyTransport RBUS where
  lazyTransmit = lazyTransmit'
