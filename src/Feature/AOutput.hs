{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.AOutput where

import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State  (MonadState)
import           Core.Actions
import           Core.Context
import qualified Core.Domain          as D
import           Core.Task
import qualified Core.Transport       as T
import           Data.Buffer
import           Data.Fixed
import           Data.Index
import           Data.Record
import           Data.Serialize
import           Data.Value           hiding (value)
import qualified Endpoint.AOutputs   as E
import           GHC.TypeNats
import qualified Interface.DAC        as I
import           Interface.MCU
import           Ivory.Language
import           Ivory.Stdlib
import           Support.Cast


data AOutputs n = forall a. I.DAC a => AOutputs
    { n              :: Uint8
    , getAOtputs     :: E.AOutputs n
    , getDACs        :: List n a
    , shouldInit     :: Value IBool
    , current        :: Index Uint8
    , transmit       :: forall l. KnownNat l
                     => Buffer l Uint8 -> forall s t. Ivory (ProcEffects s t) ()
    }


aoutputs :: ( MonadState Context m
              , MonadReader (D.Domain p c) m
              , T.Transport t, I.DAC a
              , KnownNat n
              )
              => List n (p -> m a)
              -> t -> m (AOutputs n)
aoutputs dacs transport = do
    mcu              <- asks D.mcu
    shouldInit       <- asks D.shouldInit
    as               <- traverse ($ peripherals mcu) dacs
    let n             = length as
    getAOtputs       <- E.mkAOutputs "aotputs"
    current          <- index "current_analog"

    let aoutputs = AOutputs { n = fromIntegral n
                                , getAOtputs
                                , getDACs = as
                                , shouldInit
                                , current
                                , transmit = T.transmitBuffer transport
                                }

    addSync "analog_aoutputs" $ forceSync aoutputs
    addTask $ yeld "analog_aoutputs_sync" $ sync aoutputs
    addTask $ yeld "analog_aoutputs_manage" $ manage aoutputs

    pure aoutputs



manage :: KnownNat n => AOutputs n -> Ivory eff ()
manage AOutputs{..} = zipWithM_ zip getDACs ints
    where
        zip :: I.DAC a => a -> Int -> Ivory eff ()
        zip dac i = do
            let ix = fromIntegral i
            let ao = E.aoutputs getAOtputs ! ix
            v  <- deref $ ao ~> E.value
            I.setReduced dac v


forceSync :: KnownNat n => AOutputs n -> Ivory eff ()
forceSync AOutputs{..} =
    arrayMap $ \ix -> store (E.aoutputs getAOtputs ! ix ~> E.synced) false


sync :: KnownNat n => AOutputs n -> Ivory (ProcEffects s ()) ()
sync AOutputs{..} = do
    shouldInit' <- deref shouldInit
    when (iNot shouldInit') $ do
        i <- deref current
        let d = E.aoutputs getAOtputs ! toIx i
        synced' <- deref $ d ~> E.synced
        when (iNot synced') $ do
            msg <- E.message getAOtputs (i .% n)
            transmit msg
            store (d ~> E.synced) true
        store current $ i + 1


onInit :: (KnownNat l, KnownNat n)
        => AOutputs n -> Buffer l Uint8 -> Uint8
        -> Ivory (ProcEffects s t) ()
onInit AOutputs{..} buff size = 
        when (size >=? 1 + n) $ do
            offset <- local $ ival 1
            let aos = E.aoutputs getAOtputs
            arrayMap $ \ix -> do
                offset' <- deref offset
                let ao = aos ! ix
                v <- unpack buff offset' :: Ivory eff Uint8
                store (E.aoutputs getAOtputs ! ix ~> E.value) (safeCast v / 255)
                store offset $ offset' + 1
            store shouldInit false


onDim :: (KnownNat n, KnownNat l)
      => AOutputs n -> Buffer l Uint8 -> Uint8
      -> Ivory eff ()
onDim AOutputs{..} buff size = do
    when (size >=? 3) $ do
        shouldInit' <- deref shouldInit
        when (iNot shouldInit') $ do
            index <- deref $ buff ! 1
            when (index >=? 1 .&& index <=? n) $ do
                let index' = index - 1
                action <- deref $ buff ! 2
                cond_ [ action ==? 0 ==> onOff   getAOtputs index'
                      , action ==? 1 ==> onOn    getAOtputs index'
                      , action ==? 2 ==> onSet   getAOtputs index' buff size
                      ]


onDo :: (KnownNat n, KnownNat l)
    => AOutputs n -> Buffer l Uint8 -> Uint8
    -> Ivory eff ()
onDo AOutputs{..} buff size = do
    when (size >=? 3) $ do
        shouldInit' <- deref shouldInit
        when (iNot shouldInit') $ do
            index <- deref $ buff ! 1
            when (index >=? 1 .&& index <=? n) $ do
                let index' = index - 1
                value' <- deref $ buff ! 2
                ifte_ (value' ==? 0)
                    (onOn  getAOtputs index')
                    (onOff getAOtputs index')


onOn :: KnownNat n => E.AOutputs n -> Uint8 -> Ivory eff ()
onOn = E.on


onOff :: KnownNat n => E.AOutputs n -> Uint8 -> Ivory eff ()
onOff = E.off


onSet :: (KnownNat n, KnownNat l)
      => E.AOutputs n -> Uint8 -> Buffer l Uint8 -> Uint8
      -> Ivory eff ()
onSet aoutputs' index buff size =
    when (size >=? 4) $ do
        value <- unpack buff 3  :: Ivory eff Uint8
        E.set aoutputs' index (safeCast value / 255)
