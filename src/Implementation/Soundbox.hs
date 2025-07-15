{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}

module Implementation.Soundbox where

import           Control.Monad            (void)
import           Control.Monad.Reader     (MonadReader, asks)
import           Control.Monad.State      (MonadState)
import           Core.Context
import           Core.Controller
import           Core.Domain              as D
import           Core.Handler
import           Core.Task
import           Data.Buffer
import           Data.ElasticQueue
import qualified Data.Queue               as Q
import           Data.Record
import           Data.Serialize
import           Data.Value
import           Device.GD32F4xx
import           Feature.RTP
import           Feature.SPDIF
import qualified Feature.SRC4392          as S
import           GHC.TypeNats
import           Interface.ENET
import           Interface.GPIO.Output
import           Interface.GPIO.Port
import qualified Interface.I2C            as I
import           Interface.I2S
import           Interface.I2SRX
import           Interface.I2STX
import           Interface.LwipPort
import           Interface.MCU
import           Interface.SystemClock    (getSystemTime)
import           Ivory.Language
import           Ivory.Language.Uint
import           Ivory.Stdlib
import           Ivory.Stdlib.Control
import           Support.CMSIS.CoreCMFunc (disableIRQ, enableIRQ)
import           Support.Lwip.Etharp
import           Support.Lwip.Ethernet
import           Support.Lwip.IP_addr
import           Support.Lwip.Mem
import           Support.Lwip.Memp
import           Support.Lwip.Netif
import           Support.Lwip.Pbuf
import           Support.Lwip.Udp



data Soundbox i j  = Soundbox
    { i2sTrx        :: i 256 256
    , i2sTx         :: j 256

    , i2sQueue1     :: Q.Queue  512 (Records 512 SampleStruct)
    , i2sQueue2     :: Q.Queue  512 (Records 512 SampleStruct)

    , i2sSpdif      :: SPDIF 512

    , i2sRtp        :: RTP 4480

    , sampleTx1     :: Sample
    , sampleTx2     :: Sample

    , i2sSampleMix  :: Sample
    }


mkSoundbox :: ( MonadState Context m
            , MonadReader (Domain p c) m
            , Enet e, LwipPort e, Output o
            , Handler HandleI2STX  (i 256 256)
            , Handler HandleI2SRX  (i 256 256)
            , Handler HandleI2STX  (j 256)
            , I.I2C ic 2
            , Pull p d
            )
         => (p -> m e)
         -> (p -> m (i 256 256)) -> (p -> d -> m o)
         -> (p -> m (j 256)) -> (p -> d -> m o)
         -> (p -> m (ic 2)) -> (p -> d -> m o)
         -> m (Soundbox i j)
mkSoundbox enet' i2sTrx' shutdownTrx' i2sTx' shutdownTx' i2c mute = do

    src4392 <- S.mkSRC4392 i2c mute

    let name = "soudbox"
    mcu       <- asks D.mcu

    let peripherals' = peripherals mcu

    enet        <- enet'        peripherals'
    shutdownTrx <- shutdownTrx' peripherals' $ pullNone peripherals'
    shutdownTx  <- shutdownTx'  peripherals' $ pullNone peripherals'
    i2sTrx      <- i2sTrx'      peripherals'
    i2sTx       <- i2sTx'       peripherals'

    i2sQueue1      <- Q.queue (name <> "_i2s_1") =<< records_ (name <> "_i2s_buff_tx_1")
    i2sQueue2      <- Q.queue (name <> "_i2s_2") =<< records_ (name <> "_i2s_buff_tx_2")

    i2sSpdif <- mkSpdif i2sTrx

    i2sRtp   <- mkRTP enet (name <> "_rtp") (mac mcu)

    i2sSampleMix <- record (name <> "_sample_mix") [left .= izero, right .= izero]

    sampleTx1    <- record (name <> "_sample_tx1") [left .= izero, right .= izero]
    sampleTx2    <- record (name <> "_sample_tx2") [left .= izero, right .= izero]


    let soundbox = Soundbox { i2sTrx
                            , i2sTx
                            , i2sQueue1
                            , i2sQueue2
                            , i2sSpdif
                            , i2sRtp
                            , sampleTx1
                            , sampleTx2
                            , i2sSampleMix
                            }

    addModule inclEthernet
    addModule inclNetif
    addModule inclUdp
    addModule inclMem
    addModule inclMemp
    addModule inclIP_addr
    addModule inclPbuf
    addModule inclEtharp


    addInit "lanamp" $ do
        set shutdownTrx
        set shutdownTx

    addTask $ delay 1000 "eth_arp" tmrEtharp

    addTask $ yeld "refill_buff_i2s" $ refillBuffI2S soundbox


    addHandler $ HandleI2STX i2sTrx (transmitI2S soundbox)
    addHandler $ HandleI2STX i2sTx (transmitI2S2 soundbox)

    pure soundbox


refillBuffI2S :: Soundbox i j -> Ivory eff ()
refillBuffI2S s@Soundbox{..} = do
    Q.push i2sQueue1 \buff1 i -> do
        Q.push i2sQueue2 \buff2 j -> do

            spdif <- getSpdifSample i2sSpdif

            rtp <- getRtpSample i2sRtp

            mix <- mixer s spdif rtp

            buff1 ! toIx i <== mix
            buff2 ! toIx j <== mix


transmitI2S :: Soundbox i j -> Ivory eff Sample
transmitI2S Soundbox {..} = do
    Q.pop i2sQueue1 $ \ i2sBuff i -> do
        sampleTx1 <== i2sBuff ! toIx i
    pure sampleTx1


transmitI2S2 :: Soundbox i j -> Ivory eff Sample
transmitI2S2 Soundbox {..} = do
    Q.pop i2sQueue2 $ \ i2sBuff i -> do
        sampleTx2 <== i2sBuff ! toIx i
    pure sampleTx2



mixer :: Soundbox i j -> Sample -> Sample -> Ivory eff Sample
mixer amp first second = do
    fl <- deref (first ~> left)
    fr <- deref (first ~> right)
    sl <- deref (second ~> left)
    sr <- deref (second ~> right)
    store (i2sSampleMix amp ~> left)  (fl + sl)
    store (i2sSampleMix amp ~> right) (fr + sr)
    pure  (i2sSampleMix amp)
