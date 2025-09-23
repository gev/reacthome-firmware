module Feature.RS485.RBUS.Rx where

import Core.Domain (Domain (shouldInit))
import Core.Transport
import Feature.RS485.RBUS.Data
import qualified Interface.RS485 as I
import Interface.SystemClock
import Ivory.Language
import Ivory.Stdlib
import Protocol.RS485.RBUS.Master.Rx

rxHandle :: RBUS -> Ivory eff ()
rxHandle RBUS{..} = do
    store rxLock true
    store rxTimestamp =<< getSystemTime clock

rxTask :: RBUS -> Ivory (ProcEffects s ()) ()
rxTask r = do
    mode' <- deref $ mode r
    cond_
        [ mode' ==? modeRBUS ==> rxRBUS r
        , mode' ==? modeRS485 ==> rxRS485 r
        ]

rxRBUS :: RBUS -> Ivory (ProcEffects s ()) ()
rxRBUS RBUS{..} = I.receive rs $ receive protocol . castDefault

{-
    TODO: handle 16 bit values
-}
rxRS485 :: RBUS -> Ivory (ProcEffects s ()) ()
rxRS485 RBUS{..} = do
    baudrate' <- deref baudrate
    when (baudrate' >? 0) $ do
        rsSize' <- deref rsSize
        t0 <- deref rxTimestamp
        t1 <- getSystemTime clock
        let dt = 40_000 ./ baudrate' + 1 -- wait 4 bytes timeout
        I.receive rs $ \v -> do
            store (rsBuff ! toIx rsSize') $ castDefault v
            store rsSize $ rsSize' + 1
        when (rsSize' >? 0 .&& t1 - t0 >? dt) $ do
            lazyTransmit transport (rsSize' + 2) $ \transmit -> do
                transmit 0xa2
                transmit $ fromIntegral index
                for (toIx rsSize') $ \ix ->
                    transmit . castDefault =<< deref (rsBuff ! ix)
            store rsSize 0

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
    mode' <- deref mode
    t0 <- deref rxTimestamp
    t1 <- getSystemTime clock
    when (mode' ==? modeRBUS .&& t1 - t0 >? 1) $ do
        I.clearRX rs
        reset protocol
        store rxLock false
        store rxTimestamp t1
