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

import           Control.Monad            (void, zipWithM_)
import           Control.Monad.Reader     (MonadReader, asks)
import           Control.Monad.State      (MonadState)
import           Core.Context
import           Core.Controller
import           Core.Domain              as D
import           Core.Handler
import           Core.Task
import           Data.Buffer
import           Data.DoubleBuffer        (DoubleBuffer (num1))
import           Data.ElasticQueue
import qualified Data.Fixed               as F
import           Data.Foldable            (forM_)
import qualified Data.Queue               as Q
import           Data.Record
import           Data.Serialize
import           Data.Value
import           Device.GD32F4xx
import           Feature.I2SPlay
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
import           Ivory.Support
import           Support.CMSIS.CoreCMFunc (disableIRQ, enableIRQ)
import           Support.Lwip.Etharp
import           Support.Lwip.Ethernet
import           Support.Lwip.Igmp
import           Support.Lwip.IP_addr
import           Support.Lwip.Mem
import           Support.Lwip.Memp
import           Support.Lwip.Netif
import           Support.Lwip.Pbuf
import           Support.Lwip.Udp
import qualified Core.Transport as T
import Core.Actions
import qualified GHC.Base as G
import Data.ByteString (group)
import Data.Fixed (MakeFrom(from))
import Device.GD32F3x0.I2C (I2C(txBuff))



data Soundbox = Soundbox
    { i2sTxCh1     ::  I2SPlay 512
    , i2sTxCh2     ::  I2SPlay 512
    , i2sSpdif     ::  SPDIF   512
    , rtps         :: [RTP    4480]
    , netif        ::  Record NETIF_STRUCT
    , txRtpBuff       ::  Buffer  9 Uint8
    -- , groupIp      :: Record IP_ADDR_4_STRUCT
    , i2sSampleMix :: Sample
    , transmit    :: forall n s t. KnownNat n => Buffer n Uint8 -> Ivory (ProcEffects s t) ()
    }


mkSoundbox :: ( MonadState Context m
            , T.Transport t
            , MonadReader (Domain p c) m
            , Enet e, LwipPort e, Output o
            , Handler HandleI2STX  (i 256 256)
            , Handler HandleI2SRX  (i 256 256)
            , Handler HandleI2STX  (j 256)
            , I.I2C ic 2
            , Pull p d
            )
         => m t
         -> (p -> m e)
         -> (p -> m (i 256 256)) -> (p -> d -> m o)
         -> (p -> m (j 256)) -> (p -> d -> m o)
         -> (p -> m (ic 2)) -> (p -> d -> m o)
         -> m Soundbox
mkSoundbox transport' enet i2sTrx' shutdownTrx' i2sTx' shutdownTx' i2c mute = do
    transport  <- transport'

    src4392 <- S.mkSRC4392 i2c mute

    let name = "soudbox"
    mcu       <- asks D.mcu
    let peripherals' = peripherals mcu
    shutdownTrx <- shutdownTrx' peripherals' $ pullNone peripherals'
    shutdownTx  <- shutdownTx'  peripherals' $ pullNone peripherals'
    i2sTrx      <- i2sTrx'      peripherals'
    i2sTx       <- i2sTx'       peripherals'

    txRtpBuff <- buffer (name <> "_tx_rtp_buff") 

    i2sTxCh1 <- mkI2SPlay (name <> "_transmit_ch1" ) i2sTx
    i2sTxCh2 <- mkI2SPlay (name <> "_transmit_ch2")  i2sTrx

    i2sSpdif <- mkSpdif i2sTrx

    rtps  <- mapM (\i -> mkRTP (name <> "_rtp" <> show i))  [1..8]

    i2sSampleMix  <- record (name <> "_sample_mix") [left .= izero, right .= izero]

    netif <- mkNetif enet

    -- groupIp <- record_ $ name <> "_group_ipaddr4"

    let soundbox = Soundbox { i2sTxCh1
                            , i2sTxCh2
                            , i2sSpdif
                            , rtps
                            , netif
                            , txRtpBuff
                            -- , groupIp
                            , i2sSampleMix
                            , transmit = T.transmitBuffer transport
                            }



    addNetifOnUpCallback $ netifStatusCallback soundbox

    addInit "lanamp" $ do
        set shutdownTrx
        set shutdownTx

    addTask $ yeld "refill_buff_i2s" $ refillBuffI2S soundbox


    pure soundbox


netifStatusCallback :: Soundbox -> Ivory (ProcEffects s ()) ()
netifStatusCallback Soundbox{..} = do
    pure ()
    -- let make :: RTP 4480 -> Int -> Ivory (ProcEffects s ()) ()
    --     make rtp index = do
    --         let port = 2000 + fromIntegral index
    --         groupIp <- local $ istruct [addr .= ival 0]
    --         createIpAddr4 groupIp 239 1 1 $ fromIntegral index
    --         createRtpUdp rtp netif groupIp port
    -- zipWithM_ make rtps [1..]


refillBuffI2S :: Soundbox -> Ivory eff ()
refillBuffI2S s@Soundbox{..} = do
    playI2S i2sTxCh1 \ch1 -> do
        playI2S i2sTxCh2 \ch2 -> do

            rtpSamples <- mapM getRtpSample rtps

            spdif <- getSpdifSample i2sSpdif

            mix <- mixer s spdif rtpSamples

            ch1 <== mix
            ch2 <== mix


mixer :: Soundbox -> Sample -> [Sample] -> Ivory eff Sample
mixer amp spdif samples = do
    let res = i2sSampleMix amp
    res <== spdif
    forM_ samples \sample -> do
            fl <- deref (res ~> left)
            fr <- deref (res ~> right)
            sl <- deref (sample ~> left)
            sr <- deref (sample ~> right)
            store (res ~> left)  (fl + sl)
            store (res ~> right) (fr + sr)
    pure res

instance Controller Soundbox where
    handle s@Soundbox{..} buff size = do
        action <- unpack buff 0
        cond_ [ action ==? actionRtp         ==> onRtp    s   buff size
              ]

-- type: ACTION_RTP, id, index, active, group, port
onRtp Soundbox{..} buff size = do
    when (size >=? 9) $ do
            index <- deref $ buff ! 1
            when (index >=? 1 .&& index <=? 8) $ do
                active <- unpack buff 2
                ip1 <- unpack buff 3
                ip2 <- unpack buff 4
                ip3 <- unpack buff 5
                ip4 <- unpack buff 6
                port <- unpackBE buff 7
                pack txRtpBuff 0 actionRtp
                pack txRtpBuff 1 index
                pack txRtpBuff 2 active
                pack txRtpBuff 3 ip1
                pack txRtpBuff 4 ip2
                pack txRtpBuff 5 ip3
                pack txRtpBuff 6 ip4
                packLE txRtpBuff 7 port
                transmit txRtpBuff
                let run rtp i = 
                        when (index ==? fromIntegral i) $ do
                            removeRtpUdp rtp netif
                            when active $ createRtpUdp rtp netif ip1 ip2 ip3 ip4 port
                zipWithM_ run rtps [1..]
            





