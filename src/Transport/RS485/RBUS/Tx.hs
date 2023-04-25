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
txHandle RBUS{..} = store txLock false


txTask :: RBUS -> Ivory (ProcEffects s ()) ()
txTask r@RBUS{..} = do
    rxLock' <- deref rxLock
    txLock' <- deref txLock
    when (iNot rxLock' .&& iNot txLock') $ do
        ts <- getSystemTime clock

        hasAddress' <- hasAddress protocol
        ifte_ hasAddress'
            (do
                {-
                    TODO: Move initialization out of the RBUS protocol
                -}
                shouldInit' <- deref shouldInit
                when shouldInit'
                    (doRequestInit r ts)

                shouldConfirm' <- deref shouldConfirm
                ifte_ shouldConfirm'
                    (doConfirm r ts)
                    (doTransmitMessage r ts >> doPing r ts)
            )
            (doDiscovery r ts)



doTransmitMessage :: RBUS -> Uint32 -> Ivory (ProcEffects s ()) ()
doTransmitMessage r@RBUS{..} ts = peek msgQueue $ \i -> do
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
            store txTimestamp ts
            rsTransmit r size
        )
        (remove msgQueue)



doDiscovery :: RBUS -> Uint32 -> Ivory (ProcEffects s ()) ()
doDiscovery r@RBUS{..} t1 = do
    t0 <- deref txTimestamp
    when (t1 - t0 >? 1000)
         (do
            store txTimestamp t1
            toRS transmitDiscovery r
         )


doConfirm :: RBUS -> Uint32 -> Ivory (ProcEffects s ()) ()
doConfirm r@RBUS{..} t1 = do
    t0 <- deref txTimestamp
    when (t1 - t0 >? 0)
         (do store shouldConfirm false
             store txTimestamp t1
             toRS transmitConfirm r
         )


doPing :: RBUS -> Uint32 -> Ivory (ProcEffects s ()) ()
doPing r@RBUS{..} t1 = do
    t0 <- deref txTimestamp
    when (t1 - t0 >? 1000)
         (do
            store txTimestamp t1
            toRS transmitPing r
         )



doRequestInit :: RBUS -> Uint32 -> Ivory (ProcEffects s ()) ()
doRequestInit r@RBUS{..} t1 = do
    t0 <- deref initTimestamp
    when (t1 - t0 >? 2000)
         (do
            store initTimestamp t1
            toQueue r initBuff
         )



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
