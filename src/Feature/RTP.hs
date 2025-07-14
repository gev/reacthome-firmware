{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.RTP where


import           Control.Monad.State
import           Core.Context
import           Core.Task
import           Data.ElasticQueue
import           Data.Functor          (void)
import           Data.Record
import           Data.Value
import           GHC.TypeNats
import           Interface.ENET
import           Interface.I2S
import           Interface.LwipPort
import           Ivory.Language
import           Ivory.Stdlib
import           Support.Lwip.Ethernet
import           Support.Lwip.IP_addr
import           Support.Lwip.Mem
import           Support.Lwip.Memp
import           Support.Lwip.Netif
import           Support.Lwip.Pbuf
import           Support.Lwip.Udp


data RTP n = RTP {
     name     :: String
   , rtpQueue :: ElasticQueue n (Records n SampleStruct)
   , i2sWord  :: Sample
}

mkRTP :: ( MonadState Context m
         , LwipPort e, KnownNat n, Enet e) =>
         e -> String ->
         Values 6 Uint8 -> m (RTP n)
mkRTP enet name mac = do

    ip4       <- record_ $ name <> "_ipaddr4"
    netmask   <- record_ $ name <> "_netmask"
    gateway   <- record_ $ name <> "_gateway"
    netif     <- record_ $ name <> "_netif"

    rtpQueue  <- elastic (name <> "_rtp_queue") =<< records_ (name <> "_rtp_buff")
    i2sWord   <- record (name <> "_rtp_word") [left .= izero, right .= izero]

    let rtp = RTP {name, rtpQueue, i2sWord}

    addProc $ udpReceiveCallback rtp
    addProc $ netifStatusCallback rtp

    addInit (name <> "_rtp_init") $ do
        initMem
        initMemp
        createIpAddr4 ip4 172 16 2 2
        createIpAddr4 netmask 255 240 0 0
        createIpAddr4 gateway 172 16 0 1


        addNetif netif ip4 netmask gateway nullPtr (initLwipPortIf enet) inputEthernetPtr
        setNetifDefault netif
        setNetifStatusCallback netif (procPtr $ netifStatusCallback rtp)
        setUpNetif netif
        store (netif ~> hwaddr_len) 6
        arrayCopy (netif ~> hwaddr) mac 0 6


    addTask $ yeld "udp_rx" $ do
        reval <- rxFrameSize enet
        when (reval >? 1) $
            void $ inputLwipPortIf enet netif

    pure rtp



netifStatusCallback :: KnownNat n => RTP n -> Def (NetifStatusCallbackFn s)
netifStatusCallback r@RTP{..} = proc (name <> "netif_callback") $ \netif -> body $ do
     flags' <- deref $ netif ~> flags
     when (flags' .& netif_flag_up /=?  0) $ do
        upcb <- newUdp
        when (upcb /=? nullPtr) $ do
          err <- bindUdp upcb ipAddrAny 2000
          when (err ==? 0) $
            recvUdp upcb (procPtr $ udpReceiveCallback r) nullPtr


udpReceiveCallback :: KnownNat n => RTP n -> Def (UdpRecvFn s1 s2 s3 s4)
udpReceiveCallback r@RTP{..} =
    proc (name <>"udp_receive_callback") $ \_ upcb pbuff addr port -> body $ do
        size <- deref (pbuff ~> len)
        when (size ==? 1292) $ do
            index <- local $ ival 12
            forever $ do
                index' <- deref index
                when (index' >=? size) breakOut
                push rtpQueue $ \i2sRtpBuff ix -> do
                    msbl <- getPbufAt pbuff index'
                    lsbl <- getPbufAt pbuff (index' + 1)
                    let wordL  = (safeCast msbl `iShiftL` 8) .| safeCast lsbl :: Uint16
                    let vall = twosComplementRep $ safeCast (twosComplementCast wordL) * 4096
                    store (i2sRtpBuff ! ix ~> left) vall
                    msbr <- getPbufAt pbuff (index' + 2)
                    lsbr <- getPbufAt pbuff (index' + 3)
                    let wordR  = (safeCast msbr `iShiftL` 8) .| safeCast lsbr :: Uint16
                    let valr = twosComplementRep $ safeCast (twosComplementCast wordR) * 4096
                    store (i2sRtpBuff ! ix ~> right) valr
                store index $ index' + 4
        ret =<< freePbuf pbuff


rtpBuffQueue :: KnownNat n => RTP n -> ElasticQueue n (Records n SampleStruct)
rtpBuffQueue r@RTP{..} = rtpQueue
