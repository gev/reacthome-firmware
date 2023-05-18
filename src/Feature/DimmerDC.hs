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
import           Data.Serialize
import           Data.Value
import           Endpoint.Dimmers     as D
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



instance Controller DimmerDC where
    handle ds buff size = do
        shouldInit' <- deref $ shouldInit ds
        pure [ size >=? 3 ==> do
                action <- deref $ buff ! 0
                cond_ [ iNot shouldInit' ==> cond_
                      [ action ==? 0x00  ==> onDo   ds buff size
                      , action ==? 0xd0  ==> onDim  ds buff size
                      ]
                      , action ==? 0xf2  ==> onInit ds buff size
                      ]
             ]



onDo :: KnownNat n => DimmerDC -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
onDo DimmerDC{..} buff size = do
    index  <- deref $ buff ! 1
    when (index >=? 1 .&& index <=? n) $
        runDimmers getDimmers $ \ds -> do
            let d = addrOf ds ! toIx index
            mode' <- deref $ d ~> D.mode
            when (mode' /=? 0) $ do
                value' <- deref $ buff ! 2
                ifte_ (value' ==? 0)
                      (store (d ~> D.value) 0)
                      (store (d ~> D.value) 255)
                store (d ~> synced) false




onDim :: KnownNat n => DimmerDC -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
onDim DimmerDC{..} buff size = do
    index  <- deref $ buff ! 1
    when (index >=? 1 .&& index <=? n) $ do
        action <- deref $ buff ! 2
        cond_ [ action ==? 0 ==> undefined -- off
              , action ==? 1 ==> undefined -- on
              , action ==? 2 ==> undefined -- set
              , action ==? 3 ==> undefined -- fade
              , action ==? 4 ==> undefined -- type
              , action ==? 5 ==> undefined -- group
              ]



onInit :: KnownNat n => DimmerDC -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s ()) ()
onInit DimmerDC{..} buff size =
    when (size >=? 1 + 12 * 4) $ do
        runDimmers getDimmers $ \ds -> do
            offset <- local $ ival 1
            arrayMap $ \ix -> do
                offset' <- deref offset
                let d = addrOf ds ! ix
                store (d ~> D.group   ) =<< unpack buff  offset'
                store (d ~> D.mode    ) =<< unpack buff (offset' + 1)
                store (d ~> D.value   ) =<< unpack buff (offset' + 2)
                store (d ~> D.velocity) =<< unpack buff (offset' + 3)
                store offset $ offset' + 4
        store shouldInit false
