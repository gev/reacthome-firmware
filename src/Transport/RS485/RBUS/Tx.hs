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
    locked <- deref txLock
    when (iNot locked) $ do
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



doConfirm :: RBUS -> Uint32 -> Ivory (ProcEffects s ()) ()
doConfirm r@RBUS{..} t1 = do
    t0 <- deref timestamp
    when (t1 - t0 >? 0)
         (do store shouldConfirm false
             store timestamp t1
             confirm r
         )


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
            store timestamp ts
            rsTransmit r size
        )
        (remove msgQueue)


doPing :: RBUS -> Uint32 -> Ivory (ProcEffects s ()) ()
doPing r@RBUS{..} t1 = do
    t0 <- deref timestamp
    when (t1 - t0 >? 1000)
         (store timestamp t1 >> ping r)


doDiscovery :: RBUS -> Uint32 -> Ivory (ProcEffects s ()) ()
doDiscovery r@RBUS{..} t1 = do
    t0 <- deref timestamp
    when (t1 - t0 >? 1000)
         (store timestamp t1 >> discovery r)


doRequestInit :: RBUS -> Uint32 -> Ivory (ProcEffects s ()) ()
doRequestInit r@RBUS{..} t1 = do
    t0 <- deref timestamp'
    when (t1 - t0 >? 2000)
         (store timestamp' t1 >> toQueue r initBuff)



ping :: RBUS -> Ivory (ProcEffects s ()) ()
ping = toRS transmitPing

discovery :: RBUS -> Ivory (ProcEffects s ()) ()
discovery = toRS transmitDiscovery

confirm :: RBUS -> Ivory (ProcEffects s ()) ()
confirm = toRS transmitConfirm



toRS :: (Slave 255 -> (Uint8 -> Ivory eff ()) -> Ivory (ProcEffects s ()) ())
     -> RBUS
     -> Ivory (ProcEffects s ()) ()
toRS transmit r@RBUS{..} = do
    locked <- deref txLock
    when (iNot locked)
         (rsTransmit r =<< run protocol transmit txBuff 0)


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
