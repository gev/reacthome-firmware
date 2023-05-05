{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE RecordWildCards    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}
{-# LANGUAGE NumericUnderscores #-}

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
    {-
        workaround
        TODO: Add error checking on receive
    -}
    txLock' <- deref txLock
    when (iNot txLock') $ do
        store rxLock true
        store rxTimestamp =<< getSystemTime clock
        push rxQueue $ \i ->
            store (rxBuff ! toIx i) value


rxTask :: RBUS -> Ivory (ProcEffects s ()) ()
rxTask RBUS{..} = do
    isRBUS' <- deref isRBUS
    ifte_ isRBUS'
        (pop rxQueue $ \i ->
            receive protocol . castDefault =<< deref (rxBuff ! toIx i)
        )
        (do
            {-
                TODO: handle 16 bit values
            -}
            pop rxQueue $ \i -> do
                rsSize' <- deref rsSize
                store (rsBuff ! toIx rsSize') =<< deref (rxBuff ! toIx i)
                store rsSize $ rsSize' + 1
            rsSize'   <- deref rsSize
            t0        <- deref rxTimestamp
            t1        <- getSystemTime clock
            baudrate' <- deref baudrate
            let dt     = 40_000 ./ baudrate' -- wait 4 bytes timeout
            when (rsSize' >? 0 .&& t1 - t0 >? dt) $ do
                lazyTransmit transport $ \transmit -> do
                    transmit $ rsSize' + 2
                    transmit 0xa2
                    transmit $ fromIntegral index
                    for (toIx rsSize') $ \ix ->
                        transmit . castDefault =<< deref (rsBuff ! ix)
                store rsSize 0
        )


resetTask :: RBUS -> Ivory eff ()
resetTask RBUS{..} = do
    t0 <- deref rxTimestamp
    t1 <- getSystemTime clock
    when (t1 - t0 >? 1) $ do
        reset protocol
        store rxLock false
