{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeOperators       #-}

module Implementation.Lanamp where

import           Control.Monad         (void)
import           Control.Monad.Reader  (MonadReader, asks)
import           Control.Monad.State   (MonadState)
import           Core.Context
import           Core.Controller
import           Core.Domain           as D
import           Core.Handler
import           Core.Task
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Record
import           Data.Serialize
import           Data.Value
import           Device.GD32F4xx
import           Device.GD32F4xx.I2STX (I2STX)
import           GHC.TypeNats
import           Interface.ENET
import           Interface.I2STX
import           Interface.LwipPort
import           Interface.MCU
import           Interface.SystemClock (getSystemTime)
import           Ivory.Language
import           Ivory.Language.Uint
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





data Lanamp t = Lanamp
    { i2sTx    :: I2STX  t
    , i2sBuff  :: Buffer 3000 Uint32
    , i2sQueue :: Queue  3000
    , i2sWord  :: Value Uint32
    , rtpBuff  :: Buffer 1292 Uint8
    }


receive :: Lanamp t -> Uint32 -> Ivory eff ()
receive (Lanamp {..}) word =
    push i2sQueue $ \i -> do
        store (i2sBuff ! toIx i) word


mkLanamp :: (MonadState Context m, MonadReader (Domain p c) m, Enet e, LwipPort e)
      => (p -> m e) -> (p -> m (I2STX 32)) -> m (Lanamp 32)
mkLanamp enet i2sTx' = do
    let name = "lanamp"
    mcu       <- asks D.mcu
    enet'     <- enet $ peripherals mcu
    ip4       <- record_ "ipaddr4"
    netmask   <- record_ "netmask"
    gateway   <- record_ "gateway"
    netif     <- record_ "netif"

    i2sTx       <-  i2sTx' $ peripherals mcu
    i2sBuff     <-  values' (name <> "_i2sBuff") 0
    i2sQueue    <-  queue   (name <> "_i2sQueue")
    rtpBuff     <-  values' (name <> "_rtpBuff") 0
    i2sWord     <-  value (name <> "_word1") 0


    let lanamp = Lanamp { i2sTx
                        , i2sBuff
                        , i2sQueue
                        , i2sWord
                        , rtpBuff
                        }

    addModule inclEthernet
    addModule inclNetif
    addModule inclUdp
    addModule inclMem
    addModule inclMemp
    addModule inclIP_addr
    addModule inclPbuf
    addModule inclEtharp

    addProc $ udpReceiveCallback lanamp
    addProc $ netifStatusCallback lanamp

    addInit "lanamp" $ do
        initMem
        initMemp
        createIpAddr4 ip4 172 16 2 1
        createIpAddr4 netmask 255 240 0 0
        createIpAddr4 gateway 172 16 0 1
        store (netif ~> hwaddr_len) 6
        arrayCopy (netif ~> hwaddr) (mac mcu) 0 6

        addNetif netif ip4 netmask gateway nullPtr (initLwipPortIf enet') inputEthernetPtr
        setNetifDefault netif
        setNetifStatusCallback netif (procPtr $ netifStatusCallback lanamp)
        setUpNetif netif

    -- addHandler $ HandleEnet enet' $ do
    --     reval <- rxFrameSize enet'
    --     when (reval >? 1) $
    --         void $ inputLwipPortIf enet' netif

    addTask $ yeld "udp_rx" $ do 
        reval <- rxFrameSize enet'
        when (reval >? 1) $ 
            void $ inputLwipPortIf enet' netif

    addTask $ delay 1000 "eth_arp" tmrEtharp


    addHandler $ HandleI2STX i2sTx (transmitI2S lanamp)
    pure lanamp




netifStatusCallback :: Lanamp t -> Def (NetifStatusCallbackFn s)
netifStatusCallback lanamp = proc "netif_callback" $ \netif -> body $ do
     flags' <- deref $ netif ~> flags
     when (flags' .& netif_flag_up /=?  0) $ do
        upcb <- newUdp
        when (upcb /=? nullPtr) $ do
          err <- bindUdp upcb ipAddrAny 2000
          when (err ==? 0) $
            recvUdp upcb (procPtr $ udpReceiveCallback lanamp) nullPtr



udpReceiveCallback :: Lanamp t -> Def (UdpRecvFn s1 s2 s3 s4)
udpReceiveCallback (Lanamp {..}) = proc "udp_echo_callback" $ \_ upcb pbuff addr port -> body $ do
    size <- deref (pbuff ~> tot_len)
    when (size >? 0 .&& size <=? arrayLen rtpBuff) $ do
        for (toIx size) $ \ix ->
            store (rtpBuff ! ix) =<< getPbufAt pbuff (castDefault $ fromIx ix)

        let n = 640 :: Uint16
        for (toIx n)  $ \ix ->
            unpackLE rtpBuff ((ix * 2) + 12) >>= pushRTPBuffToQueue (Lanamp {..})

    ret =<< freePbuf pbuff


pushRTPBuffToQueue :: Lanamp t -> Uint16 -> Ivory eff ()
pushRTPBuffToQueue (Lanamp {..}) word =
    push i2sQueue $ \i -> do
        store (i2sBuff ! toIx i) $ safeCast word


transmitI2S :: Lanamp t -> Ivory eff Uint32
transmitI2S (Lanamp {..}) = do
    pop i2sQueue $ \i -> do
        store i2sWord =<< deref (i2sBuff ! toIx i)
    deref i2sWord
