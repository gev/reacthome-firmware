{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Feature.Dimmer.DC where

import           Control.Monad        (zipWithM_)
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State  (MonadState)
import           Core.Context
import           Core.Controller
import qualified Core.Domain          as D
import           Core.Feature
import           Core.Task
import           Core.Transport       as T
import           Data.Buffer
import           Data.Index
import           Data.Record
import           Data.Serialize
import           Data.Value
import           Endpoint.Dimmers     as Dim
import           GHC.TypeNats
import           Interface.MCU
import qualified Interface.PWM        as I
import           Ivory.Language
import           Ivory.Stdlib
import           Support.Cast


data DimmerDC = forall p. I.PWM p => DimmerDC
    { n          :: Uint8
    , getDimmers :: Dimmers
    , getPWMs    :: [p]
    , shouldInit :: Value IBool
    , current    :: Index Uint8
    , transmit   :: forall n. KnownNat n
                 => Buffer n Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }

mkDimmerDC :: ( MonadState Context m
              , MonadReader (D.Domain p t) m
              , T.Transport t, I.PWM o
              ) => [p -> Uint32 -> Uint32 -> m o] -> m DimmerDC
mkDimmerDC pwms = do
    mcu         <- asks D.mcu
    transport   <- asks D.transport
    shouldInit  <- asks D.shouldInit
    os          <- mapM (\pwm -> pwm (peripherals mcu) 1_000_000 1_000) pwms
    let n        = length os
    getDimmers  <- dimmers "dimmers" n
    current     <- index "current_dimmer"

    let dimmerDC = DimmerDC { n = fromIntegral n
                            , getDimmers
                            , getPWMs = os
                            , shouldInit
                            , current
                            , transmit = T.transmitBuffer transport
                            }

    addTask $ delay 1 "dimmers_manage" $ manage dimmerDC
    addTask $ yeld    "dimmers_sync"   $ sync dimmerDC

    addSync "dimmers" $ forceSyncDimmerDC dimmerDC

    pure dimmerDC


forceSyncDimmerDC :: DimmerDC -> Ivory eff ()
forceSyncDimmerDC dimmers = runDimmers (getDimmers dimmers) $ \rs ->
        arrayMap $ \ix -> store (addrOf rs ! ix ~> synced) false



dimmerDC :: ( MonadState Context m
            , MonadReader (D.Domain p t) m
            , T.Transport t, I.PWM o
            ) => [p -> Uint32 -> Uint32 -> m o] -> m Feature
dimmerDC pwms = Feature <$> mkDimmerDC pwms


manage :: DimmerDC -> Ivory eff ()
manage DimmerDC{..} = zipWithM_ zip getPWMs [0..]
    where
        zip :: I.PWM p => p -> Int -> Ivory eff ()
        zip pwm i = runDimmers getDimmers $ \ds -> do
            let ix = fromIntegral i
            let d = addrOf ds ! ix
            manageDimmer pwm d



manageDimmer :: I.PWM p => p -> Record DimmerStruct -> Ivory eff ()
manageDimmer pwm dimmer = do
    calculateValue dimmer
    v <- deref $ dimmer ~> Dim.value
    cond_ [ v ==? 0 ==> I.setMode pwm I.FORCE_LOW
          , v ==? 1 ==> I.setMode pwm I.FORCE_HIGH
          , true ==> do I.setMode pwm I.LOW
                        I.setDuty pwm =<< castFloatToUint16 ((1 - v) * 960 + 40)
          ]



sync :: DimmerDC -> Ivory (ProcEffects s ()) ()
sync DimmerDC{..} = do
    shouldInit' <- deref shouldInit
    when (iNot shouldInit') $ do
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
        action <- deref $ buff ! 0
        pure [ action ==? 0x00 ==> onDo       ds buff size
             , action ==? 0xd0 ==> onDim      ds buff size
             , action ==? 0xf2 ==> onInit     ds buff size
             , action ==? 0xf4 ==> onGetState ds
             ]



onDo :: KnownNat n => DimmerDC -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
onDo DimmerDC{..} buff size = do
    when (size >=? 3) $ do
        shouldInit' <- deref shouldInit
        when (iNot shouldInit') $ do
            index <- deref $ buff ! 1
            when (index >=? 1 .&& index <=? n) $ do
                let index' = index - 1
                value' <- deref $ buff ! 2
                ifte_ (value' ==? 0)
                    (onOn  getDimmers index')
                    (onOff getDimmers index')



onDim :: KnownNat n => DimmerDC -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
onDim DimmerDC{..} buff size = do
    when (size >=? 3) $ do
        shouldInit' <- deref shouldInit
        when (iNot shouldInit') $ do
            index <- deref $ buff ! 1
            when (index >=? 1 .&& index <=? n) $ do
                let index' = index - 1
                action <- deref $ buff ! 2
                cond_ [ action ==? 0 ==> onOff   getDimmers index'
                      , action ==? 1 ==> onOn    getDimmers index'
                      , action ==? 2 ==> onSet   getDimmers index' buff size
                      , action ==? 3 ==> onFade  getDimmers index' buff size
                      , action ==? 4 ==> onMode  getDimmers index' buff size
                      , action ==? 5 ==> onGroup getDimmers index' buff size
                      ]



onInit :: KnownNat n => DimmerDC -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onInit DimmerDC{..} buff size =
    when (size >=? 1 + n * 3) $ do
        offset <- local $ ival 1
        runDimmers getDimmers $ \ds -> do
            arrayMap $ \ix -> do
                offset' <- deref offset
                let d = addrOf ds ! ix
                group    <- unpack buff  offset'
                mode     <- unpack buff (offset' + 1)
                value    <- unpack buff (offset' + 2) :: Ivory eff Uint8
                initialize d group mode (safeCast value / 255) 0
                syncDimmerGroup ds d ix
                store offset $ offset' + 3
        store shouldInit false



onOn :: Dimmers -> Uint8 -> Ivory eff ()
onOn = on


onOff :: Dimmers -> Uint8 -> Ivory eff ()
onOff = off


onSet :: KnownNat n => Dimmers -> Uint8 -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
onSet dimmers index buff size =
    when (size >=? 4) $ do
        brightness <- unpack buff 3  :: Ivory eff Uint8
        setBrightness (safeCast brightness / 255) dimmers index


onFade :: KnownNat n => Dimmers -> Uint8 -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
onFade dimmers index buff size =
    when (size >=? 5) $ do
        value    <- unpack buff 3 :: Ivory eff Uint8
        velocity <- unpack buff 4 :: Ivory eff Uint8
        fade (safeCast value / 255) (safeCast velocity / 255) dimmers index


onMode :: KnownNat n => Dimmers -> Uint8 -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
onMode dimmers index buff size =
    when (size >=? 4) $ do
        mode <- unpack buff 3
        setMode mode dimmers index


onGroup :: KnownNat n => Dimmers -> Uint8 -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
onGroup dimmers index buff size =
    when (size >=? 4) $ do
        group <- unpack buff 3
        setGroup group dimmers index


onGetState = forceSyncDimmerDC
