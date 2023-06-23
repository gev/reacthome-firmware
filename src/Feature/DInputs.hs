{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.DInputs where

import           Control.Monad        (zipWithM_)
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Writer (MonadWriter)
import           Core.Context
import           Core.Controller
import qualified Core.Domain          as D
import           Core.Feature
import           Core.Task
import qualified Core.Transport       as T
import           Data.Buffer
import           Data.Index
import           Data.Record
import           Data.Serialize
import           Data.Value
import qualified Endpoint.DInputs     as DI
import           GHC.TypeNats
import           Interface.GPIO.Input
import           Interface.MCU        (MCU, peripherals, systemClock)
import           Ivory.Language
import           Ivory.Stdlib



data DInputs = forall i. Input i => DInputs
    { n          :: Uint8
    , getDInputs :: DI.DInputs
    , getInputs  :: [i]
    , current    :: Index Uint8
    , transmit   :: forall n. KnownNat n
                 => Buffer n Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }



mkDInputs :: (MonadWriter Context m, MonadReader (D.Domain p t) m, T.Transport t, Input i)
          => [p -> m i] -> m DInputs
mkDInputs inputs = do
    mcu        <- asks D.mcu
    transport  <- asks D.transport
    is         <- mapM ($ peripherals mcu) inputs
    let n       = length is
    getDInputs <- DI.dinputs "dinputs" n
    current    <- index "current_dinput"
    pure DInputs { n = fromIntegral n
                 , getDInputs
                 , getInputs = is
                 , current
                 , transmit = T.transmitBuffer transport
                 }



dinputs :: (MonadWriter Context m, MonadReader (D.Domain p t) m, T.Transport t, Input i)
        => [p -> m i] -> m Feature
dinputs inputs = do
    dinputs <-  mkDInputs inputs
    addTask  $ delay 10 "dinputs_manage" $ manageDInputs dinputs
    addTask  $ yeld     "dinputs_sync"   $ syncDInputs   dinputs
    pure     $ Feature dinputs



instance Controller DInputs



manageDInputs :: DInputs -> Ivory eff ()
manageDInputs DInputs{..} = zipWithM_ zip getInputs [0..]
    where
        zip :: Input i => i -> Int -> Ivory eff ()
        zip input i = DI.runDInputs getDInputs $ \rs -> do
            let ix = fromIntegral i
            let r = addrOf rs ! ix
            manageDInput r input



manageDInput :: Input i
             => Record DI.DInputStruct
             -> i
             -> Ivory eff ()
manageDInput di i  = do
    value <- iNot <$> get i
    state <- deref $ di ~> DI.state
    when (value /=? state) $ do
        store (di ~> DI.state ) value
        store (di ~> DI.synced) false



syncDInputs :: DInputs -> Ivory (ProcEffects s ()) ()
syncDInputs rs@DInputs{..} = do
    i <- deref current
    syncDInput rs i
    store current $ i + 1



syncDInput :: DInputs -> Uint8 -> Ivory (ProcEffects s ()) ()
syncDInput DInputs{..} i =
    DI.runDInputs getDInputs $ \rs -> do
        let r = addrOf rs ! toIx i
        synced <- deref $ r ~> DI.synced
        when (iNot synced) $ do
            msg <- DI.message getDInputs (i .% n)
            transmit msg
            store (r ~> DI.synced) true
