{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE BlockArguments #-}

module Implementation.Lanamp where

import           Control.Monad                (void)
import           Control.Monad.Reader         (MonadReader, asks)
import           Control.Monad.State          (MonadState)
import           Core.Context
import           Core.Controller
import           Core.Domain                  as D
import           Core.Handler
import           Core.Task
import           Data.Buffer
import           Data.Record
import           Data.Serialize
import           Data.Value
import           Device.GD32F4xx
import           GHC.TypeNats
import           Interface.ENET
import           Interface.GPIO.Output
import           Interface.GPIO.Port
import           Interface.I2S
import           Interface.I2STX
import           Interface.LwipPort
import           Interface.MCU
import           Interface.SystemClock        (getSystemTime)
import           Ivory.Language
import           Ivory.Language.Uint
import           Ivory.Stdlib
import           Ivory.Stdlib.Control
import           Support.CMSIS.CoreCMFunc     (disableIRQ, enableIRQ)
import           Support.Lwip.Etharp
import           Support.Lwip.Ethernet
import           Support.Lwip.IP_addr
import           Support.Lwip.Mem
import           Support.Lwip.Memp
import           Support.Lwip.Netif
import           Support.Lwip.Pbuf
import           Support.Lwip.Udp
import           Interface.I2SRX
import qualified Interface.I2C as I
import qualified Feature.SRC4392 as S
import qualified Data.Queue as Q
import           Data.ElasticQueue



data Lanamp i  = Lanamp
    { i2sTrx        :: i

    , i2sQueue      :: Q.Queue  256 (Records 256 SampleStruct)

    , i2sSpdifQueue :: Q.Queue  256 (Records 256 SampleStruct)

    , i2sRtpQueue   :: ElasticQueue 4480
    , i2sRtpBuff    :: Records 4480 SampleStruct

    , i2sWord       :: Sample
    , i2sWord1      :: Sample
    , i2sWord2      :: Sample

    , i2sWordMix    :: Sample
    }


type I2S i = i 192 256

mkLanamp :: ( MonadState Context m
            , MonadReader (Domain p c) m
            , Enet e, LwipPort e, Output o
            , Handler HandleI2STX (I2S i)
            , Handler HandleI2SRX (I2S i)
            , I.I2C ic 2
            , Pull p d
            )
         => (p -> m e) -> (p -> m (I2S i))
         -> (p -> d -> m o) -> (p -> m (ic 2))
         -> (p -> d -> m o) -> m (Lanamp (I2S i))
mkLanamp enet' i2sTrx' shutdown' i2c mute = do

    src4392 <- S.mkSRC4392 i2c mute

    let name = "lanamp"
    mcu       <- asks D.mcu
    enet      <- enet' $ peripherals mcu
    ip4       <- record_ "ipaddr4"
    netmask   <- record_ "netmask"
    gateway   <- record_ "gateway"
    netif     <- record_ "netif"
    shutdown  <- shutdown' (peripherals mcu) $ pullNone (peripherals mcu)
    i2sTrx     <- i2sTrx' $ peripherals mcu

    i2sQueue  <- Q.queue (name <> "_i2s") =<< records_ (name <> "_i2s_buff_l")

    i2sSpdifQueue <- Q.queue  (name <> "_i2s_spdif_queue") =<< records_ (name <> "_i2s_buff_spdif")

    i2sRtpQueue   <- elastic  (name <> "_i2s_rtp_queue")
    i2sRtpBuff    <-  records_ (name <> "_i2s_rtp_buff")

    i2sWord    <- record (name <> "_word") [left .= izero, right .= izero]
    i2sWordMix <- record (name <> "_word_mix") [left .= izero, right .= izero]
    i2sWord1   <- record (name <> "_word1") [left .= izero, right .= izero]
    i2sWord2   <- record (name <> "_word2") [left .= izero, right .= izero]

    let lanamp = Lanamp { i2sTrx
                        , i2sQueue
                        , i2sSpdifQueue
                        , i2sRtpQueue
                        , i2sRtpBuff
                        , i2sWord
                        , i2sWordMix
                        , i2sWord1
                        , i2sWord2
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
        createIpAddr4 ip4 172 16 2 2
        createIpAddr4 netmask 255 240 0 0
        createIpAddr4 gateway 172 16 0 1
        store (netif ~> hwaddr_len) 6
        arrayCopy (netif ~> hwaddr) (mac mcu) 0 6

        addNetif netif ip4 netmask gateway nullPtr (initLwipPortIf enet) inputEthernetPtr
        setNetifDefault netif
        setNetifStatusCallback netif (procPtr $ netifStatusCallback lanamp)
        setUpNetif netif

        set shutdown


    addTask $ yeld "udp_rx" $ do
        reval <- rxFrameSize enet
        when (reval >? 1) $
            void $ inputLwipPortIf enet netif

    addTask $ delay 1000 "eth_arp" tmrEtharp

    addTask $ yeld "refill_buff_i2s" $ refillBuffI2S lanamp


    addHandler $ HandleI2STX i2sTrx (transmitI2S lanamp)
    addHandler $ HandleI2SRX i2sTrx (receiveI2S lanamp)

    pure lanamp


refillBuffI2S :: Lanamp (I2S i) -> Ivory eff ()
refillBuffI2S l@Lanamp{..} = do
    Q.push i2sQueue\ buff i -> do


        Q.pop i2sSpdifQueue $ \ i2sSpdifBuff j -> do
            store (i2sWord1 ~> left) =<< deref (i2sSpdifBuff  ! toIx j ~> left) 
            store (i2sWord1 ~> right) =<< deref (i2sSpdifBuff ! toIx j ~> right)
        pop i2sRtpQueue $ \ l -> do
            store (i2sWord2 ~> left) =<< deref (i2sRtpBuff  ! toIx l ~> left) 
            store (i2sWord2 ~> right) =<< deref (i2sRtpBuff ! toIx l ~> right)

        mix <- mixer l i2sWord1 i2sWord2

        store (buff ! toIx i ~> left) =<< deref (mix ~> left)
        store (buff ! toIx i ~> right) =<< deref (mix ~> right)



netifStatusCallback :: Lanamp (I2S i) -> Def (NetifStatusCallbackFn s)
netifStatusCallback lanamp = proc "netif_callback" $ \netif -> body $ do
     flags' <- deref $ netif ~> flags
     when (flags' .& netif_flag_up /=?  0) $ do
        upcb <- newUdp
        when (upcb /=? nullPtr) $ do
          err <- bindUdp upcb ipAddrAny 2000
          when (err ==? 0) $
            recvUdp upcb (procPtr $ udpReceiveCallback lanamp) nullPtr



udpReceiveCallback :: Lanamp (I2S i) -> Def (UdpRecvFn s1 s2 s3 s4)
udpReceiveCallback l@Lanamp{..} =
    proc "udp_receive_callback" $ \_ upcb pbuff addr port -> body $ do
        size <- deref (pbuff ~> len)
        when (size ==? 1292) $ do
            index <- local $ ival 12
            forever $ do
                index' <- deref index
                when (index' >=? size) breakOut
                push i2sRtpQueue $ \ix -> do
                    msbl <- getPbufAt pbuff index'
                    lsbl <- getPbufAt pbuff (index' + 1)
                    let wordL  = (safeCast msbl `iShiftL` 8) .| safeCast lsbl :: Uint16
                    let vall = twosComplementRep $ safeCast (twosComplementCast wordL) * 1024
                    store (i2sRtpBuff ! ix ~> left) vall
                    msbr <- getPbufAt pbuff (index' + 2)
                    lsbr <- getPbufAt pbuff (index' + 3)
                    let wordR  = (safeCast msbr `iShiftL` 8) .| safeCast lsbr :: Uint16
                    let valr = twosComplementRep $ safeCast (twosComplementCast wordR) * 1024
                    store (i2sRtpBuff ! ix ~> right) valr
                store index $ index' + 4
        ret =<< freePbuf pbuff


transmitI2S :: Lanamp (I2S i) -> Ivory eff Sample
transmitI2S l@Lanamp {..} = do
    Q.pop i2sQueue $ \ i2sBuff i -> do
        store (i2sWord ~> left) =<< deref (i2sBuff ! toIx i ~> left)
        store (i2sWord ~> right)=<< deref (i2sBuff ! toIx i ~> right)
    pure i2sWord


receiveI2S :: Lanamp (I2S i) -> Sample -> Ivory eff ()
receiveI2S (Lanamp {..}) word =
    Q.push i2sSpdifQueue $ \ i2sSpdifBuff i -> do
        store (i2sSpdifBuff ! toIx i ~> left) =<< deref (word ~> left)
        store (i2sSpdifBuff ! toIx i ~> right) =<< deref (word ~> right)


mixer :: Lanamp (I2S i) -> Sample -> Sample -> Ivory eff Sample
mixer amp first second = do
    fl <- deref (first ~> left)
    fr <- deref (first ~> right)
    sl <- deref (second ~> left)
    sr <- deref (second ~> right)
    store (i2sWordMix amp ~> left) $ twosComplementRep (twosComplementCast fl + twosComplementCast sl)
    store (i2sWordMix amp ~> right) $ twosComplementRep (twosComplementCast fr + twosComplementCast sr)
    pure (i2sWordMix amp)
