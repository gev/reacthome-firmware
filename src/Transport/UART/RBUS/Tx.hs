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
import           Transport.UDP.RBUS       (discoveryTask)



discoveryTask :: (KnownNat q, KnownNat l) => RBUS q l -> Ivory (ProcEffects s t) ()
discoveryTask rbus@RBUS{..} = toQueue rbus discoveryBuff



txHandle :: RBUS q l -> Ivory eff ()
txHandle RBUS{..} = store txLock false



txTask :: (KnownNat q, KnownNat l) 
       => RBUS q l -> Ivory (ProcEffects s ()) ()
txTask r@RBUS{..} = do
    txLock' <- deref txLock
    when (iNot txLock') $
        pop msgQueue $ \i -> do
            let ix = toIx i
            offset <- deref $ msgOffset ! ix
            size   <- deref $ msgSize ! ix
            U.transmit uart $ \write ->
                for (toIx size) $ \dx -> do
                    let sx = dx + toIx offset
                    write . safeCast =<< deref (msgBuff ! sx)
            store txLock true



toQueue :: (KnownNat q, KnownNat l, KnownNat n)
        => RBUS q l
        -> Buffer n Uint8
        -> Ivory (ProcEffects s t) ()
toQueue RBUS{..} buff = push msgQueue $ \i -> do
    index <- deref msgIndex
    size  <- run protocol (transmitMessage buff) msgBuff index
    store msgIndex $ index + safeCast size
    let ix = toIx i
    store (msgOffset ! ix) index
    store (msgSize   ! ix) size


toQueue' :: (KnownNat q, KnownNat l)
         => RBUS q l
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



run :: KnownNat n
    => U.RBUS 255
    -> ((Uint8 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s t) ())
    -> Buffer n Uint8
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
