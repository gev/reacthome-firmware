{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Transport.RBUS.Tx where

import           Data.Buffer
import           Data.Concurrent.Queue
import           GHC.TypeNats
import qualified Interface.RS485        as RS
import           Interface.SystemClock
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RBUS          (messageTTL)
import           Protocol.RBUS.Slave
import           Protocol.RBUS.Slave.Tx
import           Transport.RBUS.Data


txHandle :: RBUS -> Ivory eff ()
txHandle (RBUS {..}) = store (addrOf txLock) false


txTask :: RBUS -> Ivory (ProcEffects s ()) ()
txTask r@(RBUS {..}) = do
    locked <- deref $ addrOf txLock
    when (iNot locked) $ do
        ts <- getSystemTime clock

        hasAddress' <- hasAddress protocol
        ifte_ hasAddress'
            (do
                shouldInit' <- deref $ addrOf shouldInit
                when shouldInit'
                    (doRequestInit r ts)

                shouldConfirm' <- deref $ addrOf shouldConfirm
                ifte_ shouldConfirm'
                    (doConfirm r ts)
                    (doTransmitMessage r ts >> doPing r ts)
            )
            (doDiscovery r ts)



doConfirm :: RBUS -> Uint32 -> Ivory (ProcEffects s ()) ()
doConfirm r@(RBUS {..}) ts = do
    ts' <- deref $ addrOf timestamp
    when (ts - ts' >? 0)
         (do store (addrOf shouldConfirm) false
             store (addrOf timestamp) ts
             confirm r
         )


doTransmitMessage :: RBUS -> Uint32 -> Ivory (ProcEffects s ()) ()
doTransmitMessage r@(RBUS {..}) ts = do
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
doPing r@(RBUS {..}) ts = do
    let timestamp' = addrOf timestamp
    ts' <- deref timestamp'
    when (ts - ts' >? 1000)
         (store timestamp' ts >> ping r)


doDiscovery :: RBUS -> Uint32 -> Ivory (ProcEffects s ()) ()
doDiscovery r@(RBUS {..}) ts = do
    let timestamp' = addrOf timestamp
    ts' <- deref timestamp'
    when (ts - ts' >? 1000)
         (store timestamp' ts >> discovery r)


doRequestInit r@(RBUS {..}) ts = do
    let timestamp' = addrOf timestamp
    ts' <- deref timestamp'
    when (ts - ts' >? 1000)
         (store timestamp' ts >> toQueue r initBuff)



ping :: RBUS -> Ivory (ProcEffects s ()) ()
ping = toRS transmitPing

discovery :: RBUS -> Ivory (ProcEffects s ()) ()
discovery = toRS transmitDiscovery

confirm :: RBUS -> Ivory (ProcEffects s ()) ()
confirm = toRS transmitConfirm



toRS :: (Slave 255 -> (Uint8 -> Ivory eff ()) -> Ivory (ProcEffects s ()) ())
     -> RBUS
     -> Ivory (ProcEffects s ()) ()
toRS transmit r@(RBUS {..}) = do
    locked <- deref $ addrOf txLock
    when (iNot locked)
         (rsTransmit r =<< run protocol transmit txBuff 0)


{--
    TODO: potential msgBuff overflow
--}
toQueue :: KnownNat l => RBUS -> Buffer l Uint8 -> Ivory (ProcEffects s ()) ()
toQueue (RBUS {..}) buff = do
    push msgQueue $ \i -> do
        index <- deref $ addrOf msgIndex
        size <- run protocol (transmitMessage buff) msgBuff index
        store (addrOf msgIndex) $ index + size
        let ix = toIx i
        store (addrOf msgOffset ! ix) index
        store (addrOf msgSize   ! ix) size
        store (addrOf msgTTL    ! ix) messageTTL


rsTransmit :: RBUS -> Uint16 -> Ivory (ProcEffects s ()) ()
rsTransmit (RBUS {..}) size = do
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
