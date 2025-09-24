{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use for_" #-}

module Feature.RS485.RBUS.Tx where

import Control.Monad (zipWithM_)
import Data.Buffer
import Data.Queue
import Feature.RS485.RBUS.Data
import GHC.TypeNats
import Interface.Mac
import Interface.RS485 qualified as RS
import Interface.SystemClock
import Ivory.Language
import Ivory.Stdlib
import Protocol.RS485.RBUS (broadcastAddress, messageTTL)
import Protocol.RS485.RBUS.Master as P
import Protocol.RS485.RBUS.Master.Tx (
    transmitConfirm,
    transmitDiscovery,
    transmitMessage,
    transmitPing,
 )

txHandle :: RBUS -> Ivory eff ()
txHandle RBUS{..} = do
    store rxLock false
    store txLock false

txTask :: RBUS -> Ivory (ProcEffects s ()) ()
txTask r@RBUS{..} = do
    mode' <- deref mode
    rxLock' <- deref rxLock
    txLock' <- deref txLock
    when (mode' ==? modeRBUS .&& iNot rxLock' .&& iNot txLock') do
        shouldDiscovery' <- deref shouldDiscovery
        shouldConfirm' <- deref shouldConfirm
        shouldPing' <- deref shouldPing
        cond_
            [ shouldConfirm' ==> doConfirm r
            , shouldPing' ==> doPing r
            , shouldDiscovery' ==> doDiscovery r
            , true ==> doTransmitMessage r
            ]

doTransmitMessage :: RBUS -> Ivory (ProcEffects s ()) ()
doTransmitMessage r@RBUS{..} = do
    t0 <- deref txTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 1) do
        peek msgQueue \Messages{..} i -> do
            let ix = toIx i
            offset <- deref $ msgOffset ! ix
            address <- deref $ msgBuff ! toIx (offset + 1)
            let ax = toIx address
            let confirmed = msgConfirmed ! ax
            confirmed' <- deref confirmed
            store confirmed false
            ifte_
                confirmed'
                do
                    remove msgQueue
                do
                    ttl <- deref $ msgTTL ! ix
                    ifte_
                        (ttl >? 0)
                        do
                            size <- deref $ msgSize ! ix
                            RS.transmit rs \write ->
                                for (toIx size) \dx -> do
                                    let sx = dx + toIx offset
                                    write . safeCast =<< deref (msgBuff ! sx)
                            store (msgTTL ! ix) $ ttl - 1
                            store txTimestamp t1
                            store txLock true
                            store (msgWaitingConfirm ! ax) true
                        do
                            remove msgQueue

doDiscovery :: RBUS -> Ivory (ProcEffects s ()) ()
doDiscovery r@RBUS{..} = do
    store shouldDiscovery false
    address' <- deref discoveryAddress
    toRS (transmitDiscovery address') r

doConfirm :: RBUS -> Ivory (ProcEffects s ()) ()
doConfirm r@RBUS{..} = do
    store shouldConfirm false
    address' <- deref confirmAddress
    toRS (transmitConfirm address') r

doPing :: RBUS -> Ivory (ProcEffects s ()) ()
doPing r@RBUS{..} = do
    t0 <- deref txTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 5000) do
        address' <- deref pingAddress
        toRS (transmitPing address') r
        store txTimestamp t1

toRS ::
    (Master 255 -> (Uint8 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s ()) ()) ->
    RBUS ->
    Ivory (ProcEffects s ()) ()
toRS transmit r@RBUS{..} = do
    RS.transmit rs \write -> transmit protocol (write . safeCast)
    store txLock true

{--
    TODO: potential message overwriting in the msgBuff
--}
toQueue ::
    (KnownNat l) =>
    RBUS ->
    Uint8 ->
    Buffer l Uint8 ->
    Ix l ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
toQueue RBUS{..} address buff offset size = push msgQueue \Messages{..} i -> do
    index <- deref msgIndex
    size' <- run protocol (transmitMessage address buff offset size) msgBuff index
    store msgIndex $ index + safeCast size'
    let ix = toIx i
    store (msgOffset ! ix) index
    store (msgSize ! ix) size'
    store (msgTTL ! ix) messageTTL

run ::
    (KnownNat l) =>
    Master 255 ->
    (Master 255 -> (Uint8 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s t) ()) ->
    Buffer l Uint8 ->
    Uint16 ->
    Ivory (ProcEffects s t) Uint8
run protocol transmit buff offset = do
    size <- local $ ival 0
    let go :: Uint8 -> Ivory eff ()
        go v = do
            i <- deref size
            let ix = toIx $ offset + safeCast i
            store (buff ! ix) v
            store size $ i + 1
    transmit protocol go
    deref size
