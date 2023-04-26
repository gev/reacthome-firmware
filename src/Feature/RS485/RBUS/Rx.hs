{-# LANGUAGE RecordWildCards #-}

module Feature.RS485.RBUS.Rx where

import           Data.Concurrent.Queue
import           Feature.RS485.RBUS.Data
import           Interface.SystemClock
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RS485.RBUS.Master.Rx


rxHandle :: RBUS -> Uint16 -> Ivory eff ()
rxHandle RBUS{..} value = do
    push rxQueue $ \i -> do
        store rxTimestamp =<< getSystemTime clock
        store (rxBuff ! toIx i) value
        store rxLock true


rxTask :: RBUS -> Ivory (ProcEffects s ()) ()
rxTask RBUS{..} =
    pop rxQueue $ \i -> do
        v <- deref $ rxBuff ! toIx i
        receive protocol $ castDefault v


resetTask :: RBUS -> Ivory eff ()
resetTask RBUS{..} = do
    t0 <- deref rxTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 0) $ do
        reset protocol
        store rxLock false
