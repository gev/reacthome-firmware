{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use for_" #-}

module Transport.RS485.RBUS.Tx where

import Data.Buffer
import Data.Queue
import GHC.TypeNats
import qualified Interface.RS485 as RS
import Interface.SystemClock
import Ivory.Language
import Ivory.Stdlib
import Protocol.RS485.RBUS (messageTTL)
import Protocol.RS485.RBUS.Slave
import Protocol.RS485.RBUS.Slave.Tx
import Transport.RS485.RBUS.Data

txHandle :: RBUS -> Ivory eff ()
txHandle RBUS{..} = do
    store rxLock false
    store txLock false

txTask :: RBUS -> Ivory (ProcEffects s ()) ()
txTask r@RBUS{..} = do
    rxLock' <- deref rxLock
    txLock' <- deref txLock
    when (iNot rxLock' .&& iNot txLock') do
        hasAddress' <- hasAddress protocol
        ifte_
            hasAddress'
            do
                doConfirm r
                doTransmitMessage r
                doPing r
            do
                doDiscovery r

initTask :: RBUS -> Ivory (ProcEffects s ()) ()
initTask r@RBUS{..} = do
    shouldInit' <- deref shouldInit
    when shouldInit' do
        toQueue r initBuff

doTransmitMessage :: RBUS -> Ivory (ProcEffects s ()) ()
doTransmitMessage r@RBUS{..} = do
    t0 <- deref txTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 1) do
        peek msgQueue \Messages{..} ix -> do
            ttl <- deref $ msgTTL ! ix
            ifte_
                (ttl >? 0)
                do
                    offset <- deref $ msgOffset ! ix
                    size <- deref $ msgSize ! ix
                    confirmed' <- deref msgConfirmed
                    ifte_
                        confirmed'
                        do
                            store msgConfirmed false
                            remove msgQueue
                        do
                            size <- deref $ msgSize ! ix
                            RS.transmit rs \write ->
                                for (toIx size) \dx -> do
                                    let sx = dx + toIx offset
                                    write . safeCast =<< deref (msgBuff ! sx)
                            store (msgTTL ! ix) $ ttl - 1
                            store waitingConfirm true
                            store txTimestamp t1
                            store txLock true
                do
                    remove msgQueue

doDiscovery :: RBUS -> Ivory (ProcEffects s ()) ()
doDiscovery r@RBUS{..} = do
    t0 <- deref txTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 1000) do
        toRS transmitDiscovery r

doConfirm :: RBUS -> Ivory (ProcEffects s ()) ()
doConfirm r@RBUS{..} = do
    shouldConfirm' <- deref shouldConfirm
    when shouldConfirm' do
        store shouldConfirm false
        toRS transmitConfirm r

doPing :: RBUS -> Ivory (ProcEffects s ()) ()
doPing r@RBUS{..} = do
    t0 <- deref txTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 1000) do
        toRS transmitPing r

toRS ::
    (Slave 255 -> (Uint8 -> Ivory eff ()) -> Ivory (ProcEffects s ()) ()) ->
    RBUS ->
    Ivory (ProcEffects s ()) ()
toRS transmit r@RBUS{..} = do
    RS.transmit rs \write -> transmit protocol (write . safeCast)
    store txTimestamp =<< getSystemTime clock
    store txLock true

{--
    TODO: potential message overwriting in the msgBuff
--}
toQueue :: (KnownNat l) => RBUS -> Buffer l Uint8 -> Ivory (ProcEffects s t) ()
toQueue RBUS{..} buff = do
    hasAddress' <- hasAddress protocol
    when hasAddress' do
        push msgQueue \Messages{..} ix -> do
            index <- deref msgIndex
            size <- run protocol (transmitMessage buff) msgBuff index
            store msgIndex $ index + safeCast size
            store (msgOffset ! ix) index
            store (msgSize ! ix) size
            store (msgTTL ! ix) messageTTL

toQueue' ::
    RBUS ->
    Uint8 ->
    ((Uint8 -> forall eff. Ivory eff ()) -> forall eff. Ivory eff ()) ->
    Ivory (ProcEffects s t) ()
toQueue' RBUS{..} size' transmit = do
    hasAddress' <- hasAddress protocol
    when hasAddress' do
        push msgQueue \Messages{..} ix -> do
            index <- deref msgIndex
            size <- run protocol (transmitMessage' size' transmit) msgBuff index
            store msgIndex $ index + safeCast size
            store (msgOffset ! ix) index
            store (msgSize ! ix) size
            store (msgTTL ! ix) messageTTL

run ::
    (KnownNat l, SafeCast Uint8 v, IvoryStore v) =>
    Slave 255 ->
    (Slave 255 -> (Uint8 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s t) ()) ->
    Buffer l v ->
    Uint16 ->
    Ivory (ProcEffects s t) Uint8
run protocol transmit buff offset = do
    size <- local $ ival 0
    let go :: Uint8 -> Ivory eff ()
        go v = do
            i <- deref size
            let ix = toIx $ offset + safeCast i
            store (buff ! ix) $ safeCast v
            store size $ i + 1
    transmit protocol go
    deref size
