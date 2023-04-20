{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.RS485.RBUS.Tx where

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
import           Protocol.RS485.RBUS.Master
import           Protocol.RS485.RBUS.Master.Tx (transmitConfirm,
                                                transmitDiscovery,
                                                transmitMessage, transmitPing)



txHandle :: RBUS -> Ivory eff ()
txHandle RBUS{..} = store txLock false


txTask :: RBUS -> Ivory (ProcEffects s ()) ()
txTask r@RBUS{..} = do
    locked <- deref txLock
    when (iNot locked) $ do
        shouldDiscovery' <- deref shouldDiscovery
        shouldConfirm'   <- deref shouldConfirm
        shouldPing'      <- deref shouldPing
        cond_ [ shouldPing'      ==> doPing r
              , shouldConfirm'   ==> doConfirm r
              , shouldDiscovery' ==> doDiscovery r
              , true             ==> doTransmitMessage r
              ]



doTransmitMessage :: RBUS -> Ivory (ProcEffects s ()) ()
doTransmitMessage r@RBUS{..} = peek msgQueue $ \i -> do
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
    store shouldPing false
    address' <- deref pingAddress
    toRS (transmitPing address') r



toRS :: (Master 255 -> (Uint8 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s ()) ())
     -> RBUS
     -> Ivory (ProcEffects s ()) ()
toRS transmit r@RBUS{..} = do
    locked <- deref txLock
    when (iNot locked)
         (rsTransmit r =<< run protocol transmit txBuff 0)


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
