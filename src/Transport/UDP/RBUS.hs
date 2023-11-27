{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeOperators    #-}

module Transport.UDP.RBUS where

import           Control.Monad                                     (void)
import           Control.Monad.Reader                              (MonadReader,
                                                                    asks)
import           Control.Monad.State                               (MonadState)
import           Core.Context
import           Core.Controller
import           Core.Domain                                       as D
import           Core.Feature
import           Core.Handler
import           Core.Task
import           Core.Transport
import           Data.Record
import           Data.Value
import           Device.GD32F4xx
import           Interface.ENET
import           Interface.LwipPort
import           Interface.MCU
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



rbus :: (MonadState Context m, MonadReader (Domain p t) m, Enet e, LwipPort e)
      => (p -> m e) -> m RBUS
rbus enet = do
    mcu       <- asks D.mcu
    enet'     <- enet $ peripherals mcu
    ip4       <- record_ "ipaddr4"
    netmask   <- record_ "netmask"
    netif     <- record_ "netif"

    let rbus   = RBUS { netif }

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
    addProc netifStatusCallback
    addProc udpEchoReceiveCallback

    addInit "udp_init" $ do
        initMem
        initMemp
        createIpAddr4 ip4     169 254 47 94
        createIpAddr4 netmask 255 255  0  0
        store (netif ~> hwaddr_len) 6
        arrayCopy (netif ~> hwaddr) (mac mcu) 0 6
        addNetif netif ip4 netmask ipAddrAny nullPtr (initLwipPortIf enet') inputEthernetPtr
        setNetifDefault netif
        setNetifStatusCallback netif $ procPtr netifStatusCallback
        initIgmp
        startIgmp netif
        setUpNetif netif

    addHandler $ HandleEnet enet' $ do
        reval <- rxFrameSize enet'
        when (reval >? 1) $
            void $ inputLwipPortIf enet' netif

    addTask $ delay 1000 "tmr_arp"  tmrEtharp
    addTask $ delay  100 "tmr_igmp" tmrIgmp

    pure rbus



netifStatusCallback :: Def (NetifStatusCallbackFn s)
netifStatusCallback = proc "netif_callback" $ \netif -> body $ do
     flags' <- deref $ netif ~> flags
     when (flags' .& netif_flag_up /=?  0) $ do
        upcb <- newUdp
        when (upcb /=? nullPtr) $ do
          err <- bindUdp upcb ipAddrAny 2017
          when (err ==? 0) $ do
            recvUdp upcb (procPtr udpEchoReceiveCallback) nullPtr



udpEchoReceiveCallback :: Def (UdpRecvFn s1 s2 s3 s4)
udpEchoReceiveCallback = proc "udp_echo_callback" $ \_ upcb pbuff addr port -> body $ do
    connectUdp upcb addr port
    sendUdp upcb pbuff
    disconnectUdp upcb
    ret =<< freePbuf pbuff



instance Transport RBUS where
  transmitBuffer _ _ = pure ()

instance LazyTransport RBUS where
  lazyTransmit _ _ = pure ()
