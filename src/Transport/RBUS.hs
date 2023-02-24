{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Transport.RBUS    where

import           Control.Monad.Reader   (Reader, asks, runReader)
import           Core.Dispatcher        (makeDispatcher)
import           Core.Domain
import           Core.Feature
import           Core.Include
import           Core.Initialize
import           Core.Task
import           Core.Transport
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Index
import           Data.Value
import           GHC.TypeNats
import           Interface.Mac          (Mac (getMac))
import           Interface.MCU          (MCU (systemClock))
import           Interface.RS485        as RS
import           Interface.SystemClock  (SystemClock, getSystemTime)
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RBUS          (messageTTL)
import           Protocol.RBUS.Slave    (Slave, hasAddress, slave)
import           Protocol.RBUS.Slave.Rx
import           Protocol.RBUS.Slave.Tx


data RBUS = RBUS
    { name          :: String
    , clock         :: SystemClock
    , rs            :: RS485
    , protocol      :: Slave   255
    , rxBuff        :: Buffer  64 Uint16
    , rxQueue       :: Queue   64
    , msgOffset     :: Buffer  32 Uint16
    , msgSize       :: Buffer  32 Uint16
    , msgTTL        :: Buffer  32 Uint8
    , msgQueue      :: Queue   32
    , msgBuff       :: Buffer 512 Uint16
    , msgIndex      :: Value      Uint16
    , txBuff        :: Buffer 255 Uint16
    , txLock        :: Value      IBool
    , timestamp     :: Value      Uint32
    , shouldConfirm :: Value      IBool
    }


rbus :: MCU mcu => Reader mcu RS485 -> Reader (Domain mcu t) RBUS
rbus rs = do
    model       <- asks model
    version     <- asks version
    mac         <- asks mac
    mcu         <- asks mcu
    features    <- asks features
    {--
        TODO: move dispatcher outside
    --}
    let dispatch = makeDispatcher features
    let rbus     = RBUS { name          = name
                        , clock         = systemClock mcu
                        , rs            = runReader rs mcu
                        , protocol      = slave name (getMac mac) model version (onMessage dispatch rbus) (onConfirm rbus) (onDiscovery rbus)
                        , rxBuff        = buffer (name <> "_rx")
                        , rxQueue       = queue  (name <> "_rx")
                        , msgOffset     = buffer (name <> "_msg_offset")
                        , msgSize       = buffer (name <> "_msg_size")
                        , msgTTL        = buffer (name <> "_msg_ttl")
                        , msgQueue      = queue  (name <> "_msg")
                        , msgBuff       = buffer (name <> "_msg_buffer")
                        , msgIndex      = value  (name <> "_msg_index") 0
                        , txBuff        = buffer (name <> "_tx")
                        , txLock        = value  (name <> "_tx_lock") false
                        , timestamp     = value  (name <> "_timestamp") 0
                        , shouldConfirm = value  (name <> "_should_confirm") false
                        }
    pure rbus

    where name = "rbus_slave"

          onMessage dispatch (RBUS {shouldConfirm, clock, timestamp}) buff n shouldHandle = do
            store (addrOf timestamp) =<< getSystemTime clock
            when shouldHandle $ dispatch buff n
            store (addrOf shouldConfirm) true

          onDiscovery (RBUS {shouldConfirm, clock, timestamp}) = do
            store (addrOf timestamp) =<< getSystemTime clock
            store (addrOf shouldConfirm) false

          onConfirm = remove . msgQueue


instance Include RBUS where
    include r = do include $ rxBuff          r
                   include $ rxQueue         r
                   include $ msgOffset       r
                   include $ msgSize         r
                   include $ msgTTL          r
                   include $ msgQueue        r
                   include $ msgBuff         r
                   include $ msgIndex        r
                   include $ txBuff          r
                   include $ txLock          r
                   include $ timestamp       r
                   include $ shouldConfirm   r
                   include $ HandleRS485 (rs r) (rxHandle r) (txHandle r)
                   include $ protocol        r


instance Initialize RBUS where
    initialize (RBUS {rs, protocol}) = initialize rs <> initialize protocol


instance Task RBUS where
    tasks r = [ yeld    (name r <> "_rx"  ) $ rxTask r
              , delay 1 (name r <> "_tx"  ) $ txTask r
              ]


txHandle :: RBUS -> Ivory eff ()
txHandle (RBUS {txLock}) = store (addrOf txLock) false


txTask :: RBUS -> Ivory (ProcEffects s ()) ()
txTask r@(RBUS {protocol, clock, txLock, shouldConfirm}) = do
    locked <- deref $ addrOf txLock
    when (iNot locked) $ do
        ts <- getSystemTime clock
        hasAddress' <- hasAddress protocol
        ifte_ hasAddress'
            (do shouldConfirm' <- deref $ addrOf shouldConfirm
                ifte_ shouldConfirm'
                    (doConfirm r ts)
                    (doTransmitMessage r ts >> doPing r ts)
            )
            (doDiscovery r ts)



rxHandle :: RBUS -> Uint16 -> Ivory eff ()
rxHandle (RBUS {rxBuff, rxQueue}) value = do
    push rxQueue $ \i -> do
        store (addrOf rxBuff ! toIx i) value


rxTask :: RBUS -> Ivory (ProcEffects s ()) ()
rxTask (RBUS {rs, protocol, rxBuff, rxQueue, txBuff}) =
    pop rxQueue $ \i -> do
        v <- deref $ addrOf rxBuff ! toIx i
        receive protocol $ castDefault v



doConfirm :: RBUS -> Uint32 -> Ivory (ProcEffects s ()) ()
doConfirm r@(RBUS {timestamp, shouldConfirm}) ts = do
    ts' <- deref $ addrOf timestamp
    when (ts - ts' >? 0)
         (do store (addrOf shouldConfirm) false
             store (addrOf timestamp) ts
             confirm r
         )


doTransmitMessage :: RBUS -> Uint32 -> Ivory (ProcEffects s ()) ()
doTransmitMessage r@(RBUS {msgOffset, msgSize, msgTTL, msgQueue, msgBuff, txBuff, timestamp}) ts = do
    peek msgQueue $ \i -> do
        let ix = toIx i
        ttl <- deref $ addrOf msgTTL ! ix
        ifte_ (ttl >? 0)
            (do offset <- deref $ addrOf msgOffset ! ix
                size   <- deref $ addrOf msgSize ! ix
                sx     <- local $ ival offset
                for (toIx size) $ \dx -> do
                    sx' <- deref sx
                    v <- deref $ addrOf msgBuff ! toIx sx'
                    store sx $ sx' + 1
                    store (addrOf txBuff ! dx) v
                store (addrOf msgTTL ! ix) $ ttl - 1
                store (addrOf timestamp) ts
                rsTransmit r size
            )
            (remove msgQueue)


doPing :: RBUS -> Uint32 -> Ivory (ProcEffects s ()) ()
doPing r@(RBUS {timestamp}) ts = do
    let timestamp' = addrOf timestamp
    ts' <- deref timestamp'
    when (ts - ts' >? 1000)
         (store timestamp' ts >> ping r)


doDiscovery :: RBUS -> Uint32 -> Ivory (ProcEffects s ()) ()
doDiscovery r@(RBUS {timestamp}) ts = do
    let timestamp' = addrOf timestamp
    ts' <- deref timestamp'
    when (ts - ts' >? 1000)
         (store timestamp' ts >> discovery r)



ping :: RBUS -> Ivory (ProcEffects s ()) ()
ping = toRS transmitPing

discovery :: RBUS -> Ivory (ProcEffects s ()) ()
discovery = toRS transmitDiscovery

confirm :: RBUS -> Ivory (ProcEffects s ()) ()
confirm = toRS transmitConfirm



toRS :: (Slave 255 -> (Uint8 -> Ivory eff ()) -> Ivory (ProcEffects s ()) ())
     -> RBUS
     -> Ivory (ProcEffects s ()) ()
toRS transmit r@(RBUS {protocol, txBuff, txLock}) = do
    locked <- deref $ addrOf txLock
    when (iNot locked)
         (rsTransmit r =<< run protocol transmit txBuff 0)


{--
    TODO: potential msgBuff overflow
--}
toQueue :: KnownNat l => RBUS -> Buffer l Uint8 -> Ivory (ProcEffects s ()) ()
toQueue (RBUS {protocol, msgQueue, msgBuff, msgSize, msgTTL, msgIndex, msgOffset}) buff = do
    push msgQueue $ \i -> do
        index <- deref $ addrOf msgIndex
        size <- run protocol (transmitMessage buff) msgBuff index
        store (addrOf msgIndex) $ index + size
        let ix = toIx i
        store (addrOf msgOffset ! ix) index
        store (addrOf msgSize   ! ix) size
        store (addrOf msgTTL    ! ix) messageTTL


rsTransmit :: RBUS -> Uint16 -> Ivory (ProcEffects s ()) ()
rsTransmit (RBUS {rs, txBuff, txLock}) size = do
    let array = toCArray $ addrOf txBuff
    RS.transmit rs array size
    store (addrOf txLock) true


run :: KnownNat l
    => Slave 255
    -> (Slave 255 -> (Uint8 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s ()) ())
    -> Buffer l Uint16
    -> Uint16
    -> Ivory (ProcEffects s ()) Uint16
run protocol transmit buff offset = do
    size  <- local $ ival 0
    let go :: Uint8 -> Ivory eff ()
        go v = do
            i <- deref size
            let ix = toIx $ offset + i
            store (addrOf buff ! ix) $ safeCast v
            store size $ i + 1
    transmit protocol go
    deref size



instance Transport RBUS where
    transmit = toQueue
