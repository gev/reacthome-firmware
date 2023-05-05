{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.RS485.RBUS.Rx where

import           Core.Transport
import           Data.Concurrent.Queue
import           Feature.RS485.RBUS.Data
import           Interface.SystemClock
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RS485.RBUS.Master.Rx


rxHandle :: RBUS -> Uint16 -> Ivory eff ()
rxHandle RBUS{..} value = do
    store rxLock true
    store rxTimestamp =<< getSystemTime clock
    push rxQueue $ \i ->
        store (rxBuff ! toIx i) value


rxTask :: RBUS -> Ivory (ProcEffects s ()) ()
rxTask RBUS{..} = do
    isRBUS' <- deref isRBUS
    ifte_ isRBUS'
        (pop rxQueue $ \i -> do
            v <- deref $ rxBuff ! toIx i
            receive protocol $ castDefault v
        )
        (do
            size' <- size rxQueue
            t0    <- deref rxTimestamp
            t1    <- getSystemTime clock
            when (size' >? 0 .&& t1 - t0 >? 10) $
                lazyTransmit transport $ \transmit -> do
                    transmit . castDefault $ size' + 2
                    transmit 0xa2
                    transmit $ fromIntegral index
                    for (toIx size':: Ix 255) $ \jx ->
                        pop rxQueue $ \i ->
                            transmit . castDefault =<< deref (rxBuff ! toIx i)
        )


resetTask :: RBUS -> Ivory eff ()
resetTask RBUS{..} = do
    t0 <- deref rxTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 1) $ do
        reset protocol
        store rxLock false
