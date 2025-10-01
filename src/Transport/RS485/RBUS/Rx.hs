module Transport.RS485.RBUS.Rx where

import Interface.RS485 qualified as I
import Interface.SystemClock
import Ivory.Language
import Ivory.Stdlib
import Protocol.RS485.RBUS.Slave.Rx
import Transport.RS485.RBUS.Data

rxHandle :: RBUS -> Ivory eff ()
rxHandle RBUS{..} = do
    store rxLock true
    store rxTimestamp =<< getSystemTime clock

rxTask :: RBUS -> Ivory (ProcEffects s ()) ()
rxTask RBUS{..} =
    I.receive rs $ receive protocol . castDefault

errorHandle :: RBUS -> Ivory eff ()
errorHandle RBUS{..} = do
    I.clearRX rs
    reset protocol
    store rxLock false

{--
    TODO: Use IDLE and Error interrupts
--}
resetTask :: RBUS -> Ivory eff ()
resetTask RBUS{..} = do
    t0 <- deref rxTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 1) do
        I.clearRX rs
        reset protocol
        store rxLock false
        store rxTimestamp t1
