{-# LANGUAGE RecordWildCards #-}

module Transport.RS485.RBUS.Rx    where

import           Data.Concurrent.Queue
import           Ivory.Language
import           Protocol.RS485.RBUS.Slave.Rx
import           Transport.RS485.RBUS.Data


rxHandle :: RBUS -> Uint16 -> Ivory eff ()
rxHandle (RBUS {..}) value = do
    push rxQueue $ \i -> do
        store (rxBuff ! toIx i) value


rxTask :: RBUS -> Ivory (ProcEffects s ()) ()
rxTask (RBUS {..}) =
    pop rxQueue $ \i -> do
        v <- deref $ rxBuff ! toIx i
        receive protocol $ castDefault v
