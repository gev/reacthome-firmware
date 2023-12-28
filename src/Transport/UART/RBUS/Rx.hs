{-# LANGUAGE RecordWildCards #-}

module Transport.UART.RBUS.Rx    where

import           Data.Concurrent.Queue    as Q
import           Interface.SystemClock
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.UART.RBUS.Rx
import           Transport.UART.RBUS.Data


rxHandle :: RBUS -> Uint16 -> Ivory eff ()
rxHandle RBUS{..} value = do
    store rxTimestamp =<< getSystemTime clock
    push rxQueue $ \i ->
        store (rxBuff ! toIx i) $ castDefault value


rxTask :: RBUS -> Ivory (ProcEffects s ()) ()
rxTask RBUS{..} =
    pop rxQueue $ \i -> do
        v <- deref $ rxBuff ! toIx i
        receive protocol $ castDefault v


{--
    TODO: Use IDLE and Error interrupts
--}
resetTask :: RBUS -> Ivory eff ()
resetTask RBUS{..} = do
    t0 <- deref rxTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 1) $ do
        Q.clear rxQueue
        reset   protocol
        store   rxTimestamp t1
