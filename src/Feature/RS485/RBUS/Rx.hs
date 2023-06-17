{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.RS485.RBUS.Rx where

import           Core.Domain                   (Domain (shouldInit))
import           Core.Transport
import           Data.Concurrent.Queue         as Q
import           Feature.RS485.RBUS.Data
import           Interface.SystemClock
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RS485.RBUS.Master.Rx



rxHandle :: RBUS -> Uint16 -> Ivory eff ()
rxHandle RBUS{..} value = do
    store rxLock true
    store rxTimestamp =<< getSystemTime clock
    push rxQueue $ \i ->
        store (rxBuff ! toIx i) value



rxTask :: RBUS -> Ivory (ProcEffects s ()) ()
rxTask r = do
    mode' <- deref $ mode r
    cond_ [ mode' ==? modeRBUS  ==> rxRBUS  r
          , mode' ==? modeRS485 ==> rxRS485 r
          ]



rxRBUS :: RBUS -> Ivory (ProcEffects s ()) ()
rxRBUS RBUS{..} = pop rxQueue $ \i ->
    receive protocol . castDefault =<< deref (rxBuff ! toIx i)



{-
    TODO: handle 16 bit values
-}
rxRS485 :: RBUS -> Ivory (ProcEffects s ()) ()
rxRS485 RBUS{..} = do
    baudrate' <- deref baudrate
    when (baudrate' >? 0) $ do
        rsSize'   <- deref rsSize
        t0        <- deref rxTimestamp
        t1        <- getSystemTime clock
        let dt     = 40_000 ./ baudrate' + 1 -- wait 4 bytes timeout
        pop rxQueue $ \i -> do
            store (rsBuff ! toIx rsSize') . castDefault =<< deref (rxBuff ! toIx i)
            store rsSize $ rsSize' + 1
        when (rsSize' >? 0 .&& t1 - t0 >? dt) $ do
            lazyTransmit transport $ \transmit -> do
                transmit $ rsSize' + 2
                transmit 0xa2
                transmit $ fromIntegral index
                for (toIx rsSize') $ \ix ->
                    transmit . castDefault =<< deref (rsBuff ! ix)
            store rsSize 0



{--
    TODO: Use IDLE and Error interrupts
--}
resetTask :: RBUS -> Ivory eff ()
resetTask RBUS{..} = do
    mode' <- deref mode
    t0      <- deref rxTimestamp
    t1      <- getSystemTime clock
    when (mode' ==? modeRBUS .&& t1 - t0 >? 1) $ do
        Q.clear rxQueue
        reset   protocol
        store   rxLock false
        store   rxTimestamp t1
