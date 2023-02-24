{-# LANGUAGE NamedFieldPuns #-}


module Transport.RBUS.Rx    where

import           Data.Concurrent.Queue
import           Ivory.Language
import           Protocol.RBUS.Slave.Rx
import           Transport.RBUS.Data


rxHandle :: RBUS -> Uint16 -> Ivory eff ()
rxHandle (RBUS {rxBuff, rxQueue}) value = do
    push rxQueue $ \i -> do
        store (addrOf rxBuff ! toIx i) value


rxTask :: RBUS -> Ivory (ProcEffects s ()) ()
rxTask (RBUS {rs, protocol, rxBuff, rxQueue, txBuff}) =
    pop rxQueue $ \i -> do
        v <- deref $ addrOf rxBuff ! toIx i
        receive protocol $ castDefault v
