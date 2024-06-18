{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.RS485.RSM.Rx where

import           Core.Domain                   (Domain (shouldInit))
import           Core.Transport
import           Data.Concurrent.Queue         as Q
import           Feature.RS485.RSM.Data
import           Interface.SystemClock
import           Ivory.Language
import           Ivory.Stdlib




rxHandle :: RSM -> Uint16 -> Ivory eff ()
rxHandle RSM{..} value = do
    store rxLock true
    store rxTimestamp =<< getSystemTime clock
    push rxQueue $ \i ->
        store (rxBuff ! toIx i) value



rxTask :: RSM -> Ivory (ProcEffects s ()) ()
rxTask = rxRS485



{-
    TODO: handle 16 bit values
-}
rxRS485 :: RSM -> Ivory (ProcEffects s ()) ()
rxRS485 RSM{..} = do
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
            lazyTransmit transport (rsSize' + 2) $ \transmit -> do
                transmit 0xa2
                transmit $ fromIntegral index
                for (toIx rsSize') $ \ix ->
                    transmit . castDefault =<< deref (rsBuff ! ix)
            store rsSize 0
