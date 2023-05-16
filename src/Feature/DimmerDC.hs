{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.DimmerDC where

import           Control.Monad        (zipWithM_)
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Writer (MonadWriter)
import           Core.Context
import           Core.Controller
import qualified Core.Domain          as D
import           Core.Feature
import           Core.Task
import           Core.Transport       as T
import           Data.Buffer
import           Data.Index
import           Data.Value
import           Endpoint.Dimmers
import           GHC.TypeNats
import           Interface.MCU
import           Interface.PWM
import           Ivory.Language
import           Ivory.Stdlib


data DimmerDC = forall p. PWM p => DimmerDC
    { n          :: Uint8
    , getDimmers :: Dimmers
    , getPWMs    :: [p]
    , shouldInit :: Value IBool
    , current    :: Index Uint8
    , transmit   :: forall n. KnownNat n
                 => Buffer n Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }

dimmerDC :: ( MonadWriter Context m
            , MonadReader (D.Domain p t) m
            , T.Transport t, PWM o
            ) => [p -> m o] -> m Feature
dimmerDC pwms = do
    mcu        <- asks D.mcu
    transport  <- asks D.transport
    shouldInit <- asks D.shouldInit
    os         <- mapM ($ peripherals mcu) pwms
    let n       = length os
    getDimmers <- dimmers "dimmers" n
    current    <- index "current_dimmer"
    let dimmers = DimmerDC { n = fromIntegral n
                           , getDimmers
                           , getPWMs = os
                           , shouldInit
                           , current
                           , transmit = T.transmitBuffer transport
                           }
    addTask $ delay 10 "dimmers_manage" $ manage dimmers
    addTask $ yeld     "dimmers_sync"   $ sync dimmers
    pure $ Feature dimmers



manage :: DimmerDC -> Ivory eff ()
manage DimmerDC{..} = zipWithM_ zip getPWMs (iterate (+1) 0)
    where
        zip :: PWM p => p -> Sint32 -> Ivory eff ()
        zip pwm i = runDimmers getDimmers $ \ds -> do
            let ix = toIx i
            let d = addrOf ds ! ix
            manageDimmer d pwm



manageDimmer :: PWM p
            => Ref Global (Struct DimmerStruct)
            -> p
            -> Ivory eff ()
manageDimmer d pwm = undefined



sync :: DimmerDC -> Ivory (ProcEffects s ()) ()
sync DimmerDC{..} = do
    i <- deref current
    runDimmers getDimmers $ \ds -> do
        let d = addrOf ds ! toIx i
        synced' <- deref $ d ~> synced
        when (iNot synced') $ do
            msg <- message getDimmers (i .% n)
            transmit msg
            store (d ~> synced) true
    store current $ i + 1








instance Controller DimmerDC
