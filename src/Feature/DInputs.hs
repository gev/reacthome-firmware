{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.DInputs where

import           Control.Monad         (zipWithM_)
import           Control.Monad.Reader  (MonadReader, asks)
import           Control.Monad.State   (MonadState)
import           Core.Context
import qualified Core.Domain           as D
import           Core.Task
import qualified Core.Transport        as T
import           Data.Buffer
import           Data.Index
import           Data.Record
import           Data.Serialize
import           Data.Value
import qualified Endpoint.DInputs      as DI
import           GHC.TypeNats
import           Interface.GPIO.Input
import           Interface.GPIO.Port
import           Interface.MCU         (MCU, peripherals, systemClock)
import           Interface.SystemClock (SystemClock, getSystemTime)
import           Ivory.Language
import           Ivory.Stdlib



data DInputs = forall i. Input i => DInputs
    { n          :: Int
    , zero       :: IBool
    , getDInputs :: DI.DInputs
    , getInputs  :: [i]
    , current    :: Index Uint8
    , clock      :: SystemClock
    , transmit   :: forall n. KnownNat n
                 => Buffer n Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }



dinputs :: (MonadState Context m, MonadReader (D.Domain p t c) m, T.Transport t, Input i, Pull p d)
          => [p -> d -> m i] -> Bool -> m DInputs
dinputs inputs zero' = do
    mcu        <- asks D.mcu
    let clock   = systemClock mcu
    transport  <- asks D.transport
    let peripherals' = peripherals mcu
    let pull    = if zero' then pullUp else pullDown
    is         <- mapM (($ pull peripherals') . ($ peripherals')) inputs
    let n       = length is
    getDInputs <- DI.dinputs "dinputs" n
    current    <- index "current_dinput"

    let dinputs = DInputs { n
                          , zero = if zero' then true else false
                          , getDInputs
                          , getInputs = is
                          , current
                          , clock
                          , transmit = T.transmitBuffer transport
                          }

    addTask  $ delay 10 "dinputs_manage" $ manageDInputs dinputs
    addTask  $ yeld     "dinputs_sync"   $ syncDInputs   dinputs

    addSync "dinputs" $ forceSyncDInputs dinputs

    pure dinputs



forceSyncDInputs :: DInputs -> Ivory eff ()
forceSyncDInputs dinputs = DI.runDInputs (getDInputs dinputs) $
    \dis -> arrayMap $ \ix -> store (addrOf dis ! ix ~> DI.synced) false



manageDInputs :: DInputs -> Ivory eff ()
manageDInputs DInputs{..} = zipWithM_ zip getInputs [0..]
    where
        zip :: Input i => i -> Int -> Ivory eff ()
        zip input i = DI.runDInputs getDInputs $ \dis -> do
            let ix = fromIntegral i
            let di = addrOf dis ! ix
            manageDInput zero di input =<< getSystemTime clock



manageDInput :: Input i
             => IBool
             -> Record DI.DInputStruct
             -> i
             -> Uint32
             -> Ivory eff ()
manageDInput zero di input t  = do
    state0 <- deref $ di ~> DI.state
    state1 <- (/=? zero) <$> get input
    when (state1 /=? state0) $ do
        store (di ~> DI.state    ) state1
        store (di ~> DI.timestamp) t
        store (di ~> DI.synced   ) false



syncDInputs :: DInputs -> Ivory (ProcEffects s ()) ()
syncDInputs dis@DInputs{..} = do
    i <- deref current
    syncDInput dis i
    store current $ i + 1



syncDInput :: DInputs -> Uint8 -> Ivory (ProcEffects s ()) ()
syncDInput DInputs{..} i =
    DI.runDInputs getDInputs $ \dis -> do
        let di = addrOf dis ! toIx i
        synced <- deref $ di ~> DI.synced
        when (iNot synced) $ do
            msg <- DI.message getDInputs (i .% fromIntegral n)
            transmit msg
            store (di ~> DI.synced) true
