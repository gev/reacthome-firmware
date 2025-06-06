{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}

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
import           Data.Concurrent.ElasticQueue
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
import           Support.Lwip.Etharp
import           Support.Lwip.Ethernet
import           Support.Lwip.IP_addr
import           Support.Lwip.Mem
import           Support.Lwip.Memp
import           Support.Lwip.Netif
import           Support.Lwip.Pbuf
import           Support.Lwip.Udp



data Lanamp i o = Lanamp
    { i2sTx    :: i
    , pin      :: o
    , i2sBuff  :: Records (4480) SampleStruct
    , i2sQueue :: ElasticQueue  (4480)
    , i2sWord  :: Sample
    , flag     :: Value IBool
    }

type I2S i = i 640 

mkLanamp :: ( MonadState Context m
            , MonadReader (Domain p c) m
            , Enet e, LwipPort e, Output o
            , Handler HandleI2STX (I2S i), Pull p d
            )
         => (p -> m e) -> (p -> m (I2S i)) -> (p -> d -> m o) -> m (Lanamp (I2S i) o)
mkLanamp enet i2sTx' pin' = do
    let name = "lanamp"
    mcu       <- asks D.mcu
    enet'     <- enet $ peripherals mcu
    ip4       <- record_ "ipaddr4"
    netmask   <- record_ "netmask"
    gateway   <- record_ "gateway"
    netif     <- record_ "netif"
    pin       <- pin' (peripherals mcu) $ pullNone (peripherals mcu)
    i2sTx       <-  i2sTx' $ peripherals mcu
    i2sBuff     <-  records' (name <> "_i2s_buff_l") [left .= izero, right .= izero]
    i2sQueue    <-  elastic (name <> "_i2s")
    i2sWord     <-  record (name <> "_word1") [left .= izero, right .= izero]
    flag        <-  value (name <> "_flag") false


    let lanamp = Lanamp { i2sTx
                        , pin
                        , i2sBuff
                        , i2sQueue
                        , i2sWord
                        , flag
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

        addNetif netif ip4 netmask gateway nullPtr (initLwipPortIf enet') inputEthernetPtr
        setNetifDefault netif
        setNetifStatusCallback netif (procPtr $ netifStatusCallback lanamp)
        setUpNetif netif


    addTask $ yeld "udp_rx" $ do
        reval <- rxFrameSize enet'
        when (reval >? 1) $
            void $ inputLwipPortIf enet' netif

    addTask $ delay 1000 "eth_arp" tmrEtharp

    addHandler $ HandleI2STX i2sTx (transmitI2S lanamp)

    pure lanamp




netifStatusCallback :: Lanamp (I2S i) o -> Def (NetifStatusCallbackFn s)
netifStatusCallback lanamp = proc "netif_callback" $ \netif -> body $ do
     flags' <- deref $ netif ~> flags
     when (flags' .& netif_flag_up /=?  0) $ do
        upcb <- newUdp
        when (upcb /=? nullPtr) $ do
          err <- bindUdp upcb ipAddrAny 2000
          when (err ==? 0) $
            recvUdp upcb (procPtr $ udpReceiveCallback lanamp) nullPtr



udpReceiveCallback :: Lanamp (I2S i) o -> Def (UdpRecvFn s1 s2 s3 s4)
udpReceiveCallback lanamp = proc "udp_echo_callback" $ \_ upcb pbuff addr port -> body $ do
    size <- deref (pbuff ~> tot_len)
    when (size ==? 1292) $ do
        index <- local $ ival 12
        forever $ do
            index' <- deref index
            when (index' >=? size) breakOut
            msbl <- safeCast <$> getPbufAt pbuff index'
            lsbl <- safeCast <$> getPbufAt pbuff (index' + 1)
            let vall  = (msbl `iShiftL` 8) .| lsbl
            msbr <- safeCast <$> getPbufAt pbuff (index' + 2)
            lsbr <- safeCast <$> getPbufAt pbuff (index' + 3)
            let valr  = (msbr `iShiftL` 8) .| lsbr
            pushRTPBuffToQueue lanamp vall valr
            store index $ index' + 4
    ret =<< freePbuf pbuff


pushRTPBuffToQueue :: Lanamp (I2S i) o -> Uint16 -> Uint16 -> Ivory eff ()
pushRTPBuffToQueue (Lanamp {..}) wordL wordR =
    push i2sQueue $ \i -> do
        let vall = twosComplementRep $ safeCast (twosComplementCast wordL) * 4096
        store (i2sBuff ! toIx i ~> left) vall
        let valr = twosComplementRep $ safeCast (twosComplementCast wordR) * 4096
        store (i2sBuff ! toIx i ~> right) valr


transmitI2S :: Output o => Lanamp (I2S i) o -> Ivory eff Sample
transmitI2S (Lanamp {..}) = do
    pop' i2sQueue
        (\i -> do
            store (i2sWord ~> left) =<< deref (i2sBuff ! toIx i ~> left)
            store (i2sWord ~> right)=<< deref (i2sBuff ! toIx i ~> right)
        )
        (do
            flag' <- deref flag
            ifte_ flag'
                (set pin)
                (reset pin)
            store flag $ iNot flag'
        )
    pure i2sWord
