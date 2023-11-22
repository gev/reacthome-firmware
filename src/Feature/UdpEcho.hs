{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Feature.UdpEcho where

import           Control.Monad         (void)
import           Control.Monad.Reader  (MonadReader, asks)
import           Control.Monad.State   (MonadState)
import           Core.Context
import           Core.Controller
import           Core.Domain           as D
import           Core.Feature
import           Core.Handler
import           Core.Task
import           Data.Record
import           Data.Value
import           Device.GD32F4xx
import           Interface.ENET
import           Interface.LwipPort
import           Interface.MCU
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Stdlib.Control
import           Support.Lwip.Etharp
import           Support.Lwip.Ethernet
import           Support.Lwip.IP_addr
import           Support.Lwip.Mem
import           Support.Lwip.Memp
import           Support.Lwip.Netif
import           Support.Lwip.Pbuf
import           Support.Lwip.Udp
import Support.Device.GD32F4xx.LwipPort.Basic.Ethernetif



data UdpEcho = UdpEcho


mkUdpEcho :: (MonadState Context m, MonadReader (Domain p t) m, Enet e, LwipPort e)
      => (p -> m e) -> m UdpEcho
mkUdpEcho enet = do
    mcu     <- asks D.mcu
    enet'   <- enet $ peripherals mcu
    ip4     <- record_ "ipaddr4"
    netmask <- record_ "netmask"
    gateway <- record_ "gateway"
    netif   <- record_ "netif"

    addModule inclEthernet
    addModule inclEthernetif
    addModule inclNetif
    addModule inclUdp
    addModule inclMem
    addModule inclMemp
    addModule inclIP_addr
    addModule inclPbuf
    addModule inclEtharp

    addProc netifStatusCallback
    addProc udpEchoReceiveCallback

    addInit "udp_echo" $ do
        initMem
        initMemp
        createIpAddr4 ip4 192 168 88 9
        createIpAddr4 netmask 255 255 255 0
        createIpAddr4 gateway 192 168 88 1
        store (netif ~> hwaddr_len) 6
        arrayCopy (netif ~> hwaddr) (mac mcu) 0 6

        addNetif netif ip4 netmask gateway nullPtr (initLwipPortIf enet') inputEthernetPtr
        setNetifDefault netif
        setNetifStatusCallback netif (procPtr netifStatusCallback)
        setUpNetif netif

    addHandler $ HandleEnet enet' $ do
        reval <- rxFrameSize enet'
        when (reval >? 1) $
            void $ inputLwipPortIf enet' netif

    addTask $ delay 1000 "eth_arp" tmrEtharp

    pure UdpEcho



udpEcho :: (MonadState Context m, MonadReader (Domain p t) m, Enet e, LwipPort e)
      => (p -> m e) -> m Feature
udpEcho enet = do
    udpEcho <- mkUdpEcho enet
    pure $ Feature udpEcho



netifStatusCallback :: Def (NetifStatusCallbackFn s)
netifStatusCallback = proc "netif_callback" $ \netif -> body $ do
     flags' <- deref $ netif ~> flags
     when (flags' .& netif_flag_up /=?  0) $ do
        upcb <- newUdp
        when (upcb /=? nullPtr) $ do
          err <- bindUdp upcb ipAddrAny 2000
          when (err ==? 0) $
            recvUdp upcb (procPtr udpEchoReceiveCallback) nullPtr



udpEchoReceiveCallback :: Def (UdpRecvFn s1 s2 s3 s4)
udpEchoReceiveCallback = proc "udp_echo_callback" $ \_ upcb p addr port -> body $ do
    connectUdp upcb addr port
    sendUdp upcb p
    disconnectUdp upcb
    ret =<< freePbuf p




instance Controller UdpEcho

