{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Transport.RS485.RBUS.Tx where

import           Data.Buffer
import           Data.Concurrent.Queue
import           GHC.TypeNats
import qualified Interface.RS485              as RS
import           Interface.SystemClock
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RS485.RBUS          (messageTTL)
import           Protocol.RS485.RBUS.Slave
import           Protocol.RS485.RBUS.Slave.Tx
import           Transport.RS485.RBUS.Data



txHandle :: RBUS -> Ivory eff ()
txHandle RBUS{..} = do
    store rxLock false
    store txLock false


txTask :: RBUS -> Ivory (ProcEffects s ()) ()
txTask r@RBUS{..} = do
    rxLock' <- deref rxLock
    txLock' <- deref txLock
    when (iNot rxLock' .&& iNot txLock') $ do
        hasAddress' <- hasAddress protocol
        ifte_ hasAddress'
            (do
                {-
                    TODO: Move initialization out of the RBUS protocol
                -}
                shouldConfirm' <- deref shouldConfirm
                when shouldConfirm'
                    (doConfirm r)
                shouldInit' <- deref shouldInit
                when shouldInit'
                    (doRequestInit r)
                doTransmitMessage r
                doPing r
            )
            (doDiscovery r)



doTransmitMessage :: RBUS -> Ivory (ProcEffects s ()) ()
doTransmitMessage r@RBUS{..} = do
    t0 <- deref txTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 1) $ peek msgQueue $ \i -> do
        let ix = toIx i
        ttl <- deref $ msgTTL ! ix
        ifte_ (ttl >? 0)
            (do offset <- deref $ msgOffset ! ix
                size   <- deref $ msgSize ! ix
                sx     <- local $ ival offset
                for (toIx size) $ \dx -> do
                    sx' <- deref sx
                    v <- deref $ msgBuff ! toIx sx'
                    store sx $ sx' + 1
                    store (txBuff ! dx) v
                store (msgTTL ! ix) $ ttl - 1
                rsTransmit r size
            )
            (remove msgQueue)



doDiscovery :: RBUS -> Ivory (ProcEffects s ()) ()
doDiscovery r@RBUS{..} = do
    t0 <- deref txTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 1000) $
        toRS transmitDiscovery r


doConfirm :: RBUS -> Ivory (ProcEffects s ()) ()
doConfirm r@RBUS{..}= do
    store shouldConfirm false
    toRS transmitConfirm r


doPing :: RBUS -> Ivory (ProcEffects s ()) ()
doPing r@RBUS{..} = do
    t0 <- deref txTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 1000) $
        toRS transmitPing r



doRequestInit :: RBUS -> Ivory (ProcEffects s ()) ()
doRequestInit r@RBUS{..} = do
    t0 <- deref initTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 2000) $
        toQueue r initBuff



toRS :: (Slave 255 -> (Uint8 -> Ivory eff ()) -> Ivory (ProcEffects s ()) ())
     -> RBUS
     -> Ivory (ProcEffects s ()) ()
toRS transmit r@RBUS{..} =
    rsTransmit r =<< run protocol transmit txBuff 0


{--
    TODO: potential message overwriting in the msgBuff
--}
toQueue :: KnownNat l => RBUS -> Buffer l Uint8 -> Ivory (ProcEffects s ()) ()
toQueue RBUS{..} buff = push msgQueue $ \i -> do
    index <- deref msgIndex
    size <- run protocol (transmitMessage buff) msgBuff index
    store msgIndex $ index + size
    let ix = toIx i
    store (msgOffset ! ix) index
    store (msgSize   ! ix) size
    store (msgTTL    ! ix) messageTTL



rsTransmit :: RBUS -> Uint16 -> Ivory (ProcEffects s ()) ()
rsTransmit RBUS{..} size = do
    let array = toCArray txBuff
    RS.transmit rs array size
    store txTimestamp =<< getSystemTime clock
    store txLock true


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
            store (buff ! ix) $ safeCast v
            store size $ i + 1
    transmit protocol go
    deref size
