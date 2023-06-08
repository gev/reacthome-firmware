{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.RS485.RBUS.Tx where

import           Control.Monad                 (zipWithM_)
import           Data.Buffer
import           Data.Concurrent.Queue
import           Feature.RS485.RBUS.Data
import           GHC.TypeNats
import           Interface.Mac
import qualified Interface.RS485               as RS
import           Interface.SystemClock
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RS485.RBUS           (broadcastAddress, messageTTL)
import           Protocol.RS485.RBUS.Master    as P
import           Protocol.RS485.RBUS.Master.Tx (transmitConfirm,
                                                transmitDiscovery,
                                                transmitMessage, transmitPing)



txHandle :: RBUS -> Ivory eff ()
txHandle RBUS{..} = do
    store rxLock false
    store txLock false


txTask :: RBUS -> Ivory (ProcEffects s ()) ()
txTask r@RBUS{..} = do
    isRBUS' <- deref isRBUS
    rxLock' <- deref rxLock
    txLock' <- deref txLock
    when (isRBUS' .&& iNot rxLock' .&& iNot txLock') $ do
        shouldDiscovery' <- deref shouldDiscovery
        shouldConfirm'   <- deref shouldConfirm
        shouldPing'      <- deref shouldPing
        cond_ [ shouldPing'      ==> doPing r
              , shouldConfirm'   ==> doConfirm r
              , shouldDiscovery' ==> doDiscovery r
              , true             ==> doTransmitMessage r
              ]



doTransmitMessage :: RBUS -> Ivory (ProcEffects s ()) ()
doTransmitMessage r@RBUS{..} = do
    t0 <- deref txTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 1) $ peek msgQueue $ \i -> do
        let ix = toIx i
        ttl <- deref $ msgTTL ! ix
        ifte_ (ttl >? 0)
            (do offset        <- deref $ msgOffset ! ix
                address       <- deref $ msgBuff ! toIx (offset + 1)
                let confirmed  = msgConfirm ! toIx address
                confirmed'    <- deref confirmed
                ifte_ confirmed'
                    (do
                        store confirmed false
                        remove msgQueue
                    )
                    (do
                        sx    <- local $ ival offset
                        size  <- deref $ msgSize ! ix
                        for (toIx size) $ \dx -> do
                            sx' <- deref sx
                            v <- deref $ msgBuff ! toIx sx'
                            store sx $ sx' + 1
                            store (txBuff ! dx) v
                        store (msgTTL ! ix) $ ttl - 1
                        rsTransmit r size
                    )
            )
            (remove msgQueue)
        store txTimestamp t1



doDiscovery :: RBUS -> Ivory (ProcEffects s ()) ()
doDiscovery r@RBUS{..} = do
    store shouldDiscovery false
    address' <- deref discoveryAddress
    toRS (transmitDiscovery address') r 0


doConfirm :: RBUS -> Ivory (ProcEffects s ()) ()
doConfirm r@RBUS{..} = do
    store shouldConfirm false
    address' <- deref confirmAddress
    toRS (transmitConfirm address') r 0


doPing :: RBUS -> Ivory (ProcEffects s ()) ()
doPing r@RBUS{..} = do
    t0 <- deref txTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 1000) $ do
        address' <- deref pingAddress
        toRS (transmitPing address') r 0
        store txTimestamp t1



toRS :: (Master 255 -> (Uint8 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s ()) ())
     -> RBUS
     -> Uint16
     -> Ivory (ProcEffects s ()) ()
toRS transmit r@RBUS{..} offset =
    rsTransmit r =<< run protocol transmit txBuff offset


{--
    TODO: potential message overwriting in the msgBuff
--}
toQueue :: KnownNat l
        => RBUS
        -> Uint8
        -> Buffer l Uint8
        -> Ix l
        -> Uint8
        -> Ivory (ProcEffects s ()) ()
toQueue RBUS{..} address buff offset size = push msgQueue $ \i -> do
    index <- deref msgIndex
    size <- run protocol (transmitMessage address buff offset size) msgBuff index
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
    => Master 255
    -> (Master 255 -> (Uint8 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s ()) ())
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
