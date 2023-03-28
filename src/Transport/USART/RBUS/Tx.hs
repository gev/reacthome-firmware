{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Transport.USART.RBUS.Tx where

import           Data.Buffer
import           Data.Concurrent.Queue
import           GHC.TypeNats
import           Interface.SystemClock
import qualified Interface.USART           as U
import           Ivory.Language
import           Ivory.Stdlib
import qualified Protocol.USART.RBUS       as U
import           Protocol.USART.RBUS.Tx
import           Transport.USART.RBUS.Data


txHandle :: RBUS -> Ivory eff ()
txHandle RBUS{..} = store txLock false


txTask :: RBUS -> Ivory (ProcEffects s ()) ()
txTask r@RBUS{..} = do
    locked <- deref txLock
    when (iNot locked) $
        pop msgQueue $ \i -> do
            let ix = toIx i
            offset <- deref $ msgOffset ! ix
            size   <- deref $ msgSize ! ix
            sx     <- local $ ival offset
            for (toIx size) $ \dx -> do
                sx' <- deref sx
                v <- deref $ msgBuff ! toIx sx'
                store sx $ sx' + 1
                store (txBuff ! dx) v
            transmit r size

transmit :: RBUS -> Uint16 -> Ivory (ProcEffects s ()) ()
transmit RBUS{..} size = do
    let array = toCArray txBuff
    U.transmit usart array size
    store txLock true



toQueue :: KnownNat l => RBUS -> Buffer l Uint8 -> Ivory (ProcEffects s ()) ()
toQueue RBUS{..} buff = push msgQueue $ \i -> do
    index <- deref msgIndex
    size <- run protocol (transmitMessage buff) msgBuff index
    store msgIndex $ index + size
    let ix = toIx i
    store (msgOffset ! ix) index
    store (msgSize   ! ix) size


run :: KnownNat l
    => U.RBUS 255
    -> (U.RBUS 255 -> (Uint8 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s ()) ())
    -> Buffer l Uint16
    -> Uint16
    -> Ivory (ProcEffects s ()) Uint16
run protocol transmit buff offset = do
    size  <- local $ ival 0
    let go :: Uint8 -> Ivory eff ()
        go v = do
            i <- deref size
            let ix = toIx $ offset + i
            store (buff ! ix) $ safeCast v
            store size $ i + 1
    transmit protocol go
    deref size
