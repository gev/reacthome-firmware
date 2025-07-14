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

module Implementation.Soundbox where

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
import           Feature.RTP



data Soundbox i j  = Soundbox
    { i2sTrx        :: i 256 256
    , i2sTx         :: j 256

    , i2sQueue      :: Q.Queue  512 (Records 512 SampleStruct)
    , i2sQueue2     :: Q.Queue  512 (Records 512 SampleStruct)

    , i2sSpdifQueue :: Q.Queue  512 (Records 512 SampleStruct)

    , i2sRtp           :: RTP 4480

    , i2sWord       :: Sample
    , i2sWord1      :: Sample
    , i2sWord2      :: Sample

    , i2sWordMix    :: Sample
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

    let name = "lanamp"
    mcu       <- asks D.mcu

    let peripherals' = peripherals mcu

    enet        <- enet' peripherals'
    shutdownTrx <- shutdownTrx' peripherals' $ pullNone peripherals'
    shutdownTx  <- shutdownTx' peripherals' $ pullNone peripherals'
    i2sTrx      <- i2sTrx' peripherals'
    i2sTx       <- i2sTx'  peripherals'

    i2sQueue       <- Q.queue (name <> "_i2s_1") =<< records_ (name <> "_i2s_buff_tx_1")
    i2sQueue2      <- Q.queue (name <> "_i2s_2") =<< records_ (name <> "_i2s_buff_tx_2")

    i2sSpdifQueue <- Q.queue  (name <> "_i2s_spdif_queue") =<< records_ (name <> "_i2s_buff_spdif")

    i2sRtp   <- mkRTP enet (name <> "_rtp") (mac mcu)

    i2sWordMix <- record (name <> "_word_mix") [left .= izero, right .= izero]
    i2sWord    <- record (name <> "_word") [left .= izero, right .= izero]
    i2sWord1   <- record (name <> "_word1") [left .= izero, right .= izero]
    i2sWord2   <- record (name <> "_word2") [left .= izero, right .= izero]


    let soundbox = Soundbox { i2sTrx
                            , i2sTx
                            , i2sQueue
                            , i2sQueue2
                            , i2sSpdifQueue
                            , i2sRtp
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


    addInit "lanamp" $ do
        set shutdownTrx
        set shutdownTx

    addTask $ delay 1000 "eth_arp" tmrEtharp

    addTask $ yeld "refill_buff_i2s" $ refillBuffI2S soundbox


    addHandler $ HandleI2STX i2sTrx (transmitI2S soundbox)
    addHandler $ HandleI2SRX i2sTrx (receiveI2S soundbox)

    addHandler $ HandleI2STX i2sTx (transmitI2S2 soundbox)

    pure soundbox


refillBuffI2S :: Soundbox i j -> Ivory eff ()
refillBuffI2S s@Soundbox{..} = do


    Q.push i2sQueue \buff1 i -> do
        Q.push i2sQueue2 \buff2 j -> do
        

            Q.pop i2sSpdifQueue $ \buff i -> do
                store (i2sWord1 ~> left) =<< deref (buff  ! toIx i ~> left) 
                store (i2sWord1 ~> right) =<< deref (buff ! toIx i ~> right)

            pop (rtpBuffQueue i2sRtp) $ \buff i -> do
                store (i2sWord2 ~> left) =<< deref (buff  ! toIx i ~> left) 
                store (i2sWord2 ~> right) =<< deref (buff ! toIx i ~> right)

            mix <- mixer s i2sWord1 i2sWord2
            

            store (buff1 ! toIx i ~> left) =<<  deref (mix  ~> left) 
            store (buff1 ! toIx i ~> right) =<< deref (mix  ~> right)

            store (buff2 ! toIx j ~> left) =<< deref (mix  ~> left) 
            store (buff2 ! toIx j ~> right) =<< deref (mix  ~> right)




transmitI2S :: Soundbox i j -> Ivory eff Sample
transmitI2S Soundbox {..} = do
    Q.pop i2sQueue $ \ i2sBuff i -> do
        store (i2sWord ~> left) =<< deref (i2sBuff ! toIx i ~> left)
        store (i2sWord ~> right)=<< deref (i2sBuff ! toIx i ~> right)
    pure i2sWord


transmitI2S2 :: Soundbox i j -> Ivory eff Sample
transmitI2S2 Soundbox {..} = do
    Q.pop i2sQueue2 $ \ i2sBuff i -> do
        store (i2sWord ~> left) =<< deref (i2sBuff ! toIx i ~> left)
        store (i2sWord ~> right)=<< deref (i2sBuff ! toIx i ~> right)
    pure i2sWord


receiveI2S :: Soundbox i j -> Sample -> Ivory eff ()
receiveI2S Soundbox {..} word =
    Q.push i2sSpdifQueue $ \ i2sSpdifBuff i -> do
        store (i2sSpdifBuff ! toIx i ~> left) =<< deref (word ~> left)
        store (i2sSpdifBuff ! toIx i ~> right) =<< deref (word ~> right)



mixer :: Soundbox i j -> Sample -> Sample -> Ivory eff Sample
mixer amp first second = do
    fl <- deref (first ~> left)
    fr <- deref (first ~> right)
    sl <- deref (second ~> left)
    sr <- deref (second ~> right)
    store (i2sWordMix amp ~> left) $ twosComplementRep (twosComplementCast fl + twosComplementCast sl)
    store (i2sWordMix amp ~> right) $ twosComplementRep (twosComplementCast fr + twosComplementCast sr)
    pure (i2sWordMix amp)
