{-# LANGUAGE RecordWildCards #-}

module Transport.UART.RBUS.Rx    where

import           Interface.SystemClock
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.UART.RBUS.Rx
import           Transport.UART.RBUS.Data
import qualified Interface.UART as I


rxHandle :: RBUS -> Ivory eff ()
rxHandle RBUS{..} =
    store rxTimestamp =<< getSystemTime clock


rxTask :: RBUS -> Ivory (ProcEffects s ()) ()
rxTask RBUS{..} = I.receive uart $ receive protocol . castDefault


{--
    TODO: Use IDLE and Error interrupts
--}
resetTask :: RBUS -> Ivory eff ()
resetTask RBUS{..} = do
    t0 <- deref rxTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 1) $ do
        I.clearRX uart
        reset     protocol
        store     rxTimestamp t1
