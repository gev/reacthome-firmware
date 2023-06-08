{-# LANGUAGE RecordWildCards #-}

module Transport.RS485.RBUS.Rx    where

import           Data.Concurrent.Queue
import           Interface.RS485
import           Interface.SystemClock
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RS485.RBUS.Slave.Rx
import           Transport.RS485.RBUS.Data


rxHandle :: RBUS -> Uint16 -> Ivory eff ()
rxHandle RBUS{..} value = do
    store rxLock true
    store rxTimestamp =<< getSystemTime clock
    push rxQueue $ \i ->
        store (rxBuff ! toIx i) value


rxTask :: RBUS -> Ivory (ProcEffects s ()) ()
rxTask RBUS{..} =
    pop rxQueue $ \i -> do
        v <- deref $ rxBuff ! toIx i
        receive protocol $ castDefault v


resetTask :: RBUS -> Ivory eff ()
resetTask RBUS{..} = do
    t0 <- deref rxTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 10) $ do
        reset protocol
        store rxLock false
