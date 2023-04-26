{-# LANGUAGE RecordWildCards #-}

module Transport.UART.RBUS.Rx    where

import           Data.Concurrent.Queue
import           Interface.SystemClock
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.UART.RBUS.Rx
import           Transport.UART.RBUS.Data


rxHandle :: RBUS -> Uint16 -> Ivory eff ()
rxHandle RBUS{..} value = do
    push rxQueue $ \i -> do
        store rxTimestamp =<< getSystemTime clock
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
    when (t1 - t0 >? 0) $ reset protocol
