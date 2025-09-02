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
{-# HLINT ignore "Use for_" #-}

module Implementation.Soundbox where

import           Control.Monad            (void, zipWithM_)
import           Control.Monad.Reader     (MonadReader, asks)
import           Control.Monad.State      (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Controller
import           Core.Domain              as D
import           Core.Handler
import           Core.Task
import qualified Core.Transport           as T
import           Data.Buffer
import           Data.ByteString          (group)
import           Data.DoubleBuffer        (DoubleBuffer (num1))
import           Data.ElasticQueue
import           Data.Fixed               (MakeFrom (from))
import qualified Data.Fixed               as F
import           Data.Foldable            (forM_, for_)
import qualified Data.Queue               as Q
import           Data.Record
import           Data.Serialize
import           Data.Value
import           Device.GD32F3x0.I2C      (I2C (txBuff))
import           Device.GD32F4xx
import           Endpoint.StereoAMP
import           Feature.I2SPlay
import           Feature.RTP
import           Feature.SPDIF
import qualified Feature.SRC4392          as S
import           GHC.Arr                  (array)
import qualified GHC.Base                 as G
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
import           Language.Haskell.TH      (safe)
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
import Control.Arrow (Arrow(arr))
import Ivory.Language.Sint (Sint32(Sint32))



data Soundbox = Soundbox
    { i2sTxCh1     ::  I2SPlay 512
    , i2sTxCh2     ::  I2SPlay 512
    , i2sSpdif     ::  SPDIF   512
    , rtps         :: [RTP    4480]
    , netif        ::  Record NETIF_STRUCT
    , txRtpBuff    ::  Buffer  9 Uint8
    , lanampBuff   ::  Buffer  41 Uint8
    , i2sSampleMix :: Sample
    , samples      :: Records 9 SampleStruct
    , amps         :: Records 2 StereoAMPStruct
    , shouldInit   :: Value IBool
    , transmit     :: forall n s t. KnownNat n => Buffer n Uint8 -> Ivory (ProcEffects s t) ()
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

    let name = "soundbox"
    mcu       <- asks D.mcu
    shouldInit       <- asks D.shouldInit
    let peripherals' = peripherals mcu
    shutdownTrx <- shutdownTrx' peripherals' $ pullNone peripherals'
    shutdownTx  <- shutdownTx'  peripherals' $ pullNone peripherals'
    i2sTrx      <- i2sTrx'      peripherals'
    i2sTx       <- i2sTx'       peripherals'

    txRtpBuff <- buffer (name <> "_tx_rtp_buff")
    lanampBuff <- buffer (name <> "_tx_lanamp_buff")

    i2sTxCh1 <- mkI2SPlay (name <> "_transmit_ch1" ) i2sTx
    i2sTxCh2 <- mkI2SPlay (name <> "_transmit_ch2")  i2sTrx

    i2sSpdif <- mkSpdif i2sTrx

    samples <- records_ $ name <> "_samples"

    rtps  <- mapM (\i -> mkRTP (name <> "_rtp" <> show i))  [1..8]

    i2sSampleMix  <- record (name <> "_sample_mix") [left .= izero, right .= izero]

    netif <- mkNetif enet

    amps <- records_ $ name <> "_amps"


    let soundbox = Soundbox { i2sTxCh1
                            , i2sTxCh2
                            , i2sSpdif
                            , rtps
                            , netif
                            , txRtpBuff
                            , lanampBuff
                            , i2sSampleMix
                            , samples
                            , amps
                            , shouldInit
                            , transmit = T.transmitBuffer transport
                            }

    addStruct (Proxy:: Proxy ZoneAMPStruct)
    addStruct (Proxy:: Proxy StereoAMPStruct)

    addNetifOnUpCallback $ netifStatusCallback soundbox

    addInit "lanamp" $ do
        set shutdownTrx
        set shutdownTx

    addTask $ yeld "refill_buff_i2s" $ refillBuffI2S soundbox


    pure soundbox


netifStatusCallback :: Soundbox -> Ivory (ProcEffects s ()) ()
netifStatusCallback Soundbox{..} = do
    pure ()



refillBuffI2S :: Soundbox -> Ivory (ProcEffects s ()) ()
refillBuffI2S Soundbox{..} = do
    playI2S i2sTxCh1 \amp0 -> do
        playI2S i2sTxCh2 \amp1 -> do
            sample <- getSpdifSample i2sSpdif
            samples ! 0 <== sample
            zipWithM_ run rtps [1..]
            mix samples (amps ! 0) amp0
            mix samples (amps ! 1) amp1
    where
        run rtp i = do
            let ix = fromIntegral i
            sample <- getRtpSample rtp
            samples ! ix <== sample

mix :: Records 9 SampleStruct -> Record StereoAMPStruct -> Sample -> Ivory (ProcEffects s ()) ()
mix samples amp res = do
    mode' <- deref $ amp ~> mode

    cond_ [ mode' ==? 1 ==> mixLR samples amp res
          , mode' ==? 2 ==> mixRL samples amp res
          , mode' ==? 3 ==> mix11 samples amp res
          ]

mixLR :: Records 9 SampleStruct -> Record StereoAMPStruct -> Sample -> Ivory (ProcEffects s ()) ()
mixLR src amp dst = do
    dl <- local $ ival 0
    dr <- local $ ival 0
    let rules = amp ~> zone ! 0
    arrayMap $ \ix -> do
        isUsed' <- deref $ rules ~> isUsed ! ix
        when isUsed' $ do
            volume' <- deref $ rules ~> volume ! ix
            sl <- safeCast <$> deref (src ! ix ~> left)
            sr <- safeCast <$> deref (src ! ix ~> right)
            dl' <- deref dl
            dr' <- deref dr
            store dl $ dl' + volume' * sl
            store dr $ dr' + volume' * sr
    dl' <- deref dl
    dr' <- deref dr
    store (dst ~> left)  $ castDefault dl'
    store (dst ~> right) $ castDefault dr'

mixRL :: Records 9 SampleStruct -> Record StereoAMPStruct -> Sample -> Ivory (ProcEffects s ()) ()
mixRL src amp dst = do
    dl <- local $ ival 0
    dr <- local $ ival 0
    let rules = amp ~> zone ! 0
    arrayMap $ \ix -> do
        isUsed' <- deref $ rules  ~> isUsed ! ix
        when isUsed' $ do
            volume' <- deref $ rules ~> volume ! ix
            sl <- safeCast <$> deref (src ! ix ~> left)
            sr <- safeCast <$> deref (src ! ix ~> right)
            dl' <- deref dl
            dr' <- deref dr
            store dl $ dl' + volume' * sl
            store dr $ dr' + volume' * sr
    dl' <- deref dl
    dr' <- deref dr
    store (dst ~> right) $ castDefault dl'
    store (dst ~> left)  $ castDefault dr'

mix11 :: Records 9 SampleStruct -> Record StereoAMPStruct -> Sample -> Ivory (ProcEffects s ()) ()
mix11 src amp dst = do
    dl <- local $ ival (0 :: IFloat)
    dr <- local $ ival (0 :: IFloat)
    let rules = amp ~> zone ! 0
    arrayMap $ \ix -> do
        isUsed' <- deref $ rules  ~> isUsed ! ix
        when isUsed' $ do
            volume' <- deref $ rules ~> volume ! ix
            sl <- safeCast <$> deref (src ! ix ~> left)
            sr <- safeCast <$> deref (src ! ix ~> right)
            dl' <- deref dl
            store dl $ dl' + volume' * (sl + sr) 
    let rules = amp ~> zone ! 1
    arrayMap $ \ix -> do
        isUsed' <- deref $ rules  ~> isUsed ! ix
        when isUsed' $ do
            volume' <- deref $ rules ~> volume ! ix
            sl <- safeCast <$> deref (src ! ix ~> left)
            sr <- safeCast <$> deref (src ! ix ~> right)
            dr' <- deref dr
            store dr $ dr' + volume' * (sl + sr)
    dl' <- deref dl
    dr' <- deref dr
    store (dst ~> left)  $ castDefault (dl' / 2)
    store (dst ~> right) $ castDefault (dr' / 2)

instance Controller Soundbox where
    handle s@Soundbox{..} buff size = do
        action <- unpack buff 0
        cond_ [ action ==? actionRtp         ==> onRtp     s  buff size
              , action ==? actionLanamp      ==> onLanamp  s  buff size
              , action ==? actionInitialize  ==> onInit    s  buff size
              ]


onInit Soundbox{..} buff size = do
    when (size >=? 135) $ do
        arrayMap $ \kx -> do
            countUsed <- local (iarray [izeroval, izeroval])
            let base = 1 + 39 * fromIx kx
            mode' <- deref $ buff ! toIx base
            let amp = amps ! kx
            arrayMap $ \ix -> do
                let zone' = amp ~> zone ! ix
                arrayMap $ \jx -> do
                    let ux =  3 + base + 9 * fromIx ix + fromIx jx
                    isUsed' <- unpack buff $ toIx ux
                    store (zone' ~> isUsed ! jx) isUsed'
                    when isUsed' do
                        n <- deref (countUsed ! ix)
                        store (countUsed ! ix) (n + 1)
                    pack lanampBuff (toIx ux) isUsed'
                arrayMap $ \jx -> do
                    let vx = 22 + 9 * fromIx ix + fromIx jx
                    volume' <- unpack buff $ toIx vx
                    nu <- deref (countUsed ! ix)
                    when (nu /=? 0) do 
                           store (zone' ~> volume ! jx) $ safeCast volume' / nu 
                    store (lanampBuff ! toIx vx) volume'
                store (amp ~> mode) mode'

        let run rtp i = do
                let base = fromIntegral $ 79 + 7 * i :: Sint32
                active  <- unpack buff   $ toIx base
                ip1     <- unpack buff   $ toIx (base + 1)
                ip2     <- unpack buff   $ toIx (base + 2)
                ip3     <- unpack buff   $ toIx (base + 3)
                ip4     <- unpack buff   $ toIx (base + 4)
                port    <- unpackBE buff $ toIx (base + 5)
                removeRtpUdp rtp netif
                when active $ createRtpUdp rtp netif ip1 ip2 ip3 ip4 port
        zipWithM_ run rtps [0..]

        store shouldInit false

-- type: ACTION_RTP, index, active, group, port
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
            packBE txRtpBuff 7 port
            transmit txRtpBuff
            let run rtp i =
                    when (index ==? fromIntegral i) $ do
                        removeRtpUdp rtp netif
                        when active $ createRtpUdp rtp netif ip1 ip2 ip3 ip4 port
            zipWithM_ run rtps [1..]


-- size 41
-- lanamp:  ACTION_LANAMP index mode (2 byte volume??)  (active x 18) (volume x 18)
onLanamp Soundbox{..} buff size = do
    countUsed <- local (iarray [izeroval, izeroval])
    when (size >=? 41) $ do
        index <- deref $ buff ! 1
        when (index >=? 1 .&& index <=? 2) $ do
            mode' <- deref $ buff ! 2
            let amp = amps ! toIx (index - 1)
            arrayMap $ \ix -> do
                let zone' = amp ~> zone ! ix
                arrayMap $ \jx -> do
                    let ux =  5 + 9 * fromIx ix + fromIx jx
                    isUsed' <- unpack buff $ toIx ux
                    store (zone' ~> isUsed ! jx) isUsed'
                    when isUsed' do
                        n <- deref (countUsed ! ix)
                        store (countUsed ! ix) (n + 1)
                    pack lanampBuff (toIx ux) isUsed'
                arrayMap $ \jx -> do
                    let vx = 23 + 9 * fromIx ix + fromIx jx
                    volume' <- unpack buff $ toIx vx
                    nu <- deref (countUsed ! ix)
                    when (nu /=? 0) do 
                           store (zone' ~> volume ! jx) $ safeCast volume' / nu 
                    store (lanampBuff ! toIx vx) volume'
                store (amp ~> mode) mode'
            store (lanampBuff ! 2) mode'
            store (lanampBuff ! 1) index
            store (lanampBuff ! 0) actionLanamp

            transmit lanampBuff
