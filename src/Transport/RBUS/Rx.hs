{-# LANGUAGE RecordWildCards #-}

module Transport.RBUS.Rx    where

import           Data.Concurrent.Queue
import           Ivory.Language
import           Protocol.RBUS.Slave.Rx
import           Transport.RBUS.Data


rxHandle :: RBUS -> Uint16 -> Ivory eff ()
rxHandle (RBUS {..}) value = do
    push rxQueue $ \i -> do
        store (rxBuff ! toIx i) value


rxTask :: RBUS -> Ivory (ProcEffects s ()) ()
rxTask (RBUS {..}) =
    pop rxQueue $ \i -> do
        v <- deref $ rxBuff ! toIx i
        receive protocol $ castDefault v
