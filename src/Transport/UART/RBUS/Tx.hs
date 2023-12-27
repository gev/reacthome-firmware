{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Transport.UART.RBUS.Tx where

import           Data.Buffer
import           Data.Concurrent.Queue
import           GHC.TypeNats
import           Interface.SystemClock
import qualified Interface.UART           as U
import           Ivory.Language
import           Ivory.Stdlib
import qualified Protocol.UART.RBUS       as U
import           Protocol.UART.RBUS.Tx
import           Transport.UART.RBUS.Data


txHandle :: RBUS -> Ivory eff ()
txHandle RBUS{..} = store txLock false


txTask :: RBUS -> Ivory (ProcEffects s ()) ()
txTask r@RBUS{..} = do
    txLock' <- deref txLock
    when (iNot txLock') $
        pop msgQueue $ \i -> do
            let ix = toIx i
            offset <- deref $ msgOffset ! ix
            size   <- deref $ msgSize ! ix
            sx     <- local $ ival offset
            for (toIx size) $ \dx -> do
                sx' <- deref sx
                v <- deref $ msgBuff ! toIx sx'
                store sx $ sx' + 1
                store (txBuff ! dx) $ safeCast v
            transmit r $ safeCast size

transmit :: RBUS -> Uint16 -> Ivory (ProcEffects s ()) ()
transmit RBUS{..} size = do
    let array = toCArray txBuff
    U.transmit uart array $ safeCast size
    store txLock true



toQueue :: KnownNat l
        => RBUS
        -> Buffer l Uint8
        -> Ivory (ProcEffects s t) ()
toQueue RBUS{..} buff = push msgQueue $ \i -> do
    index <- deref msgIndex
    size  <- run protocol (transmitMessage buff) msgBuff index
    store msgIndex $ index + safeCast size
    let ix = toIx i
    store (msgOffset ! ix) index
    store (msgSize   ! ix) size


toQueue' :: RBUS
        -> Uint8
        -> ((Uint8 -> forall eff. Ivory eff ()) -> forall eff. Ivory eff ())
        -> Ivory (ProcEffects s t) ()
toQueue' RBUS{..} size' transmit = push msgQueue $ \i -> do
    index <- deref msgIndex
    size  <- run protocol (transmitMessage' size' transmit) msgBuff index
    store msgIndex $ index + safeCast size
    let ix = toIx i
    store (msgOffset ! ix) index
    store (msgSize   ! ix) size



run :: KnownNat l
    => U.RBUS 255
    -> ((Uint8 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s t) ())
    -> Buffer l Uint8
    -> Uint16
    -> Ivory (ProcEffects s t) Uint8
run protocol transmit buff offset = do
    size  <- local $ ival 0
    let go :: Uint8 -> Ivory eff ()
        go v = do
            i <- deref size
            let ix = toIx $ offset + safeCast i
            store (buff ! ix) $ safeCast v
            store size $ i + 1
    transmit go
    deref size
