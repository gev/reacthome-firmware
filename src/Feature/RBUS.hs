{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.RBUS    where

import           Control.Monad.Reader  (Reader, asks, runReader)
import           Core.Domain
import           Core.Feature
import           Core.Include
import           Core.Initialize
import           Data.Buffer
import           Data.Class
import           Data.Concurrent.Queue (Queue, pop, push, queue, size)
import           Data.Data             (cast)
import           Data.Index            (Index, index)
import           Data.Value
import           GHC.IO.BufferedIO     (readBuf)
import           GHC.TypeNats
import           Interface.Counter     (readCounter)
import           Interface.MCU
import           Interface.RS485       (HandleRS485 (HandleRS485), RS485,
                                        transmit)
import           Interface.SystemClock (SystemClock)
import           Ivory.Language
import           Ivory.Stdlib
import           Util.CRC16


data RBUS = RBUS
    { name        :: String
    , rs          :: RS485
    , clock       :: SystemClock
    , timestamp   :: Value      Uint32
    , rxBuff      :: Buffer  64 Uint16
    , rxQueue     :: Queue   64
    , msgSize     :: Value      Uint16
    , msgSizeBuff :: Buffer  32 Uint16
    , msgQueue    :: Queue   32
    , msgIndex    :: Index      (Ix 512)
    , msgBuff     :: Buffer 512 Uint16
    , txBuff      :: Buffer 256 Uint16
    , txLock      :: Value      IBool
    }


rbus :: MCU mcu => Int -> Reader mcu RS485 -> Reader (Domain mcu) Feature
rbus n rs = do
    mcu <- asks mcu
    pure . Feature $ RBUS { name          = name
                          , rs            = runReader rs mcu
                          , clock         = systemClock  mcu
                          , timestamp     = value  (name <> "_rx_timestamp") 0
                          , rxBuff        = buffer (name <> "_rx")
                          , rxQueue       = queue  (name <> "_rx")
                          , msgSizeBuff   = buffer (name <> "_msg_size")
                          , msgQueue      = queue  (name <> "_msg")
                          , msgSize       = value  (name <> "_msg_size") 0
                          , msgBuff       = buffer (name <> "_msg_buffer")
                          , msgIndex      = index  (name <> "_msg_index")
                          , txLock        = value  (name <> "_tx_lock") false
                          , txBuff        = buffer (name <> "_tx")
                          }
    where name = "rbus_" <> show n


instance Include RBUS where
    include r = do inclCRC16
                   include $ timestamp       r
                   include $ rxBuff          r
                   include $ rxQueue         r
                   include $ msgSizeBuff     r
                   include $ msgQueue        r
                   include $ msgSize         r
                   include $ msgIndex        r
                   include $ msgBuff         r
                   include $ txLock          r
                   include $ txBuff          r
                   include $ HandleRS485 (rs r) (rxHandle r) (txHandle r)


instance Initialize RBUS where
    initialize  = initialize . rs


instance Task RBUS where
    tasks r = [ yeld    (name r <> "_rx") $ rxTask r
              , delay 1 (name r <> "_tx") $ txTask r
              ]


txHandle :: RBUS -> Ivory eff ()
txHandle (RBUS {txLock}) = setValue txLock false


txTask :: RBUS -> Ivory (ProcEffects s ()) ()
txTask (RBUS {rs, msgIndex, msgSizeBuff, msgQueue, msgBuff, txBuff, txLock}) = do
    locked <- getValue txLock
    when (iNot locked) $ do
        pop msgQueue $ \i -> do
            size <- getItem msgSizeBuff (toIx i)
            let s = toIx size
            let s_1 = toIx $ size + 1
            let s_2 = toIx $ size - 2
            crc <- local $ istruct initCRC16
            for s $ \dx -> do
                sx <- getValue msgIndex
                v <- getItem msgBuff sx
                when (dx <? s_2) $ updateCRC16 crc $ castDefault v
                setItem txBuff dx v
                setValue msgIndex $ sx + 1
            m <- deref (crc ~> msb)
            l <- deref (crc ~> lsb)
            setItem txBuff s $ safeCast m
            setItem txBuff s_1 $ safeCast l
            let buff = toCArray $ getBuffer txBuff
            transmit rs buff (size + 2)
            setValue txLock true


rxHandle :: RBUS -> Uint16 -> Ivory eff ()
rxHandle (RBUS {clock, timestamp, rxBuff, rxQueue}) value = do
    push rxQueue $ \i -> do
        setItem rxBuff (toIx i) value
        setValue timestamp =<< readCounter clock


rxTask :: RBUS -> Ivory eff ()
rxTask r = rxData r >> split r


rxData :: RBUS -> Ivory eff ()
rxData (RBUS {rxBuff, rxQueue, msgSize, msgIndex, msgBuff}) = do
    pop rxQueue $ \i -> do
        setItem msgBuff (toIx i) =<< getItem rxBuff (toIx i)
        ms <- getValue msgSize
        setValue msgSize $ ms + 1


split :: RBUS -> Ivory eff ()
split (RBUS {clock, timestamp, msgSizeBuff, msgQueue, msgSize, msgBuff}) = do
    ms <- getValue msgSize
    t0 <- getValue timestamp
    t1 <- readCounter clock
    when (ms ==? getSize msgBuff .||
         (ms >? 0 .&& t1 - t0 >? 40)) $ do
        push msgQueue $ \i -> do
            setItem msgSizeBuff (toIx i) ms
            setValue msgSize 0
