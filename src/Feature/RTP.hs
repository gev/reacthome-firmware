{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE BlockArguments #-}

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
import Ivory.Support (ExtSymbol(symbol))


data RTP n = RTP {
     name      :: String
   , rtpQueue  :: ElasticQueue n (Records n SampleStruct)
   , rtpSample :: Sample
}

mkRTP :: ( MonadState Context m
         , LwipPort e, KnownNat n, Enet e) =>
         e -> String -> m (RTP n)
mkRTP enet name = do
    rtpQueue  <- elastic (name <> "_rtp_queue") =<< records_ (name <> "_rtp_buff")
    rtpSample <- record  (name <> "_rtp_sample") [left .= izero, right .= izero]

    let rtp = RTP {name, rtpQueue, rtpSample}

    -- addProc $ statusCallback rtp
    addProc $ udpReceiveCallback rtp


    pure rtp


    
-- statusCallback :: KnownNat n => RTP n -> Def (NetifStatusCallbackFn s)
-- statusCallback r@RTP{..} = do
--         upcb <- newUdp
--         when (upcb /=? nullPtr) $ do
--           err <- bindUdp upcb ipAddrAny port
--           when (err ==? 0) $
--             recvUdp upcb (procPtr $ udpReceiveCallback r) nullPtr

createUDP :: KnownNat n => RTP n -> Uint16 -> Ivory eff ()
createUDP r@RTP {..} port = do
        upcb <- newUdp
        when (upcb /=? nullPtr) $ do
          err <- bindUdp upcb ipAddrAny port
          when (err ==? 0) $
            recvUdp upcb (procPtr $ udpReceiveCallback r) nullPtr


udpReceiveCallback :: KnownNat n => RTP n -> Def (UdpRecvFn s1 s2 s3 s4)
udpReceiveCallback r@RTP{..} =
    proc (name <> "udp_receive_callback") $ \_ upcb pbuff addr port -> body $ do
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
                    let vall = safeCast (twosComplementCast wordL) * 256
                    store (i2sRtpBuff ! ix ~> left) vall
                    msbr <- getPbufAt pbuff (index' + 2)
                    lsbr <- getPbufAt pbuff (index' + 3)
                    let wordR  = (safeCast msbr `iShiftL` 8) .| safeCast lsbr :: Uint16
                    let valr =  safeCast (twosComplementCast wordR) * 256
                    store (i2sRtpBuff ! ix ~> right) valr
                store index $ index' + 4
        ret =<< freePbuf pbuff


getRtpSample :: KnownNat n => RTP n -> Ivory eff Sample
getRtpSample RTP{..} = do
    pop rtpQueue \buff i -> do
        rtpSample <== buff ! i
    pure rtpSample
