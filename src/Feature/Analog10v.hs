{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.Analog10v where

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
import           Endpoint.AOutputs
import           GHC.TypeNats
import qualified Interface.DAC        as I
import           Interface.MCU
import           Ivory.Language
import           Ivory.Stdlib
import           Support.Cast


data Analog10vs n = forall a. I.DAC a => Analog10vs
    { n              :: Uint8
    , getAOtputs     :: AOutputs n
    , getDACs        :: List n a
    , shouldInit     :: Value IBool
    , current        :: Index Uint8
    , transmit       :: forall l. KnownNat l
                     => Buffer l Uint8 -> forall s t. Ivory (ProcEffects s t) ()
    }


analog10vs :: ( MonadState Context m
              , MonadReader (D.Domain p c) m
              , T.Transport t, I.DAC a
              , KnownNat n
              )
              => List n (p -> m a)
              -> t -> m (Analog10vs n)
analog10vs dacs transport = do
    mcu              <- asks D.mcu
    shouldInit       <- asks D.shouldInit
    as               <- traverse ($ peripherals mcu) dacs
    let n             = length as
    getAOtputs       <- mkAOutputs "aotputs"
    current          <- index "current_analog10v"

    let analog10vs = Analog10vs { n = fromIntegral n
                                , getAOtputs
                                , getDACs = as
                                , shouldInit
                                , current
                                , transmit = T.transmitBuffer transport
                                }

    addSync "analog10vs" $ forceSync analog10vs
    addTask $ yeld "analog10vs_sync" $ sync analog10vs
    addTask $ yeld "analog10vs_manage" $ manage analog10vs

    pure analog10vs



manage :: KnownNat n => Analog10vs n -> Ivory eff ()
manage Analog10vs{..} = zipWithM_ zip getDACs ints
    where
        zip :: I.DAC a => a -> Int -> Ivory eff ()
        zip dac i = do
            let ix = fromIntegral i
            let ao = aoutputs getAOtputs ! ix
            v  <- deref $ ao ~> value
            I.setReduced dac v



forceSync :: KnownNat n => Analog10vs n -> Ivory eff ()
forceSync Analog10vs{..} =
    arrayMap $ \ix -> store (aoutputs getAOtputs ! ix ~> synced) false



sync :: KnownNat n => Analog10vs n -> Ivory (ProcEffects s ()) ()
sync Analog10vs{..} = do
    shouldInit' <- deref shouldInit
    when (iNot shouldInit') $ do
        i <- deref current
        let d = aoutputs getAOtputs ! toIx i
        synced' <- deref $ d ~> synced
        when (iNot synced') $ do
            msg <- message getAOtputs (i .% n)
            transmit msg
            store (d ~> synced) true
        store current $ i + 1


onInit :: (KnownNat l, KnownNat n)
        => Analog10vs n -> Buffer l Uint8 -> Uint8
        -> Ivory (ProcEffects s t) ()
onInit Analog10vs{..} buff size = 
        when (size >=? 1 + n) $ do
            offset <- local $ ival 1
            let aos = aoutputs getAOtputs
            arrayMap $ \ix -> do
                offset' <- deref offset
                let ao = aos ! ix
                v <- unpack buff offset' :: Ivory eff Uint8
                store (aoutputs getAOtputs ! ix ~> value) (safeCast v / 255)
                store offset $ offset' + 1
            store shouldInit false


onDim :: (KnownNat n, KnownNat l)
      => Analog10vs n -> Buffer l Uint8 -> Uint8
      -> Ivory eff ()
onDim Analog10vs{..} buff size = do
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
    => Analog10vs n -> Buffer l Uint8 -> Uint8
    -> Ivory eff ()
onDo Analog10vs{..} buff size = do
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


onOn :: KnownNat n => AOutputs n -> Uint8 -> Ivory eff ()
onOn = on


onOff :: KnownNat n => AOutputs n -> Uint8 -> Ivory eff ()
onOff = off


onSet :: (KnownNat n, KnownNat l)
      => AOutputs n -> Uint8 -> Buffer l Uint8 -> Uint8
      -> Ivory eff ()
onSet aoutputs index buff size =
    when (size >=? 4) $ do
        value <- unpack buff 3  :: Ivory eff Uint8
        set aoutputs index (safeCast value / 255)
