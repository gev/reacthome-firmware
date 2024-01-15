{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Feature.Dimmers where

import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State  (MonadState)
import           Core.Actions
import           Core.Context
import qualified Core.Domain          as D
import           Core.Task
import qualified Core.Transport       as T
import           Data.Buffer
import           Data.Index
import           Data.Record
import           Data.Serialize
import           Data.Value
import qualified Endpoint.Dimmers     as D
import           GHC.TypeNats
import           Interface.MCU
import qualified Interface.PWM        as I
import           Ivory.Language
import           Ivory.Stdlib
import           Support.Cast



data Dimmers = forall p. I.PWM p => Dimmers
    { n              :: Uint8
    , getDimmers     :: D.Dimmers
    , getPWMs        :: [p]
    , shouldInit     :: Value IBool
    , current        :: Index Uint8
    , transmit       :: forall n. KnownNat n
                     => Buffer n Uint8 -> forall s t. Ivory (ProcEffects s t) ()
    }



mkDimmers :: ( MonadState Context m
           , MonadReader (D.Domain p t c) m
           , T.Transport t, I.PWM o
           ) => [p -> Uint32 -> Uint32 -> m o] -> Uint32 -> m Dimmers
mkDimmers pwms period = do
    mcu         <- asks D.mcu
    transport   <- asks D.transport
    shouldInit  <- asks D.shouldInit
    os          <- mapM (\pwm -> pwm (peripherals mcu) 1_000_000 period) pwms
    let n        = length os
    getDimmers  <- D.mkDimmers "dimmers" n
    current     <- index "current_dimmer"

    let dimmers  = Dimmers{ n = fromIntegral n
                          , getDimmers
                          , getPWMs = os
                          , shouldInit
                          , current
                          , transmit = T.transmitBuffer transport
                          }

    addSync "dimmers" $ forceSync dimmers
    addTask $ yeld "dimmers_sync" $ sync dimmers

    pure dimmers



forceSync :: Dimmers -> Ivory eff ()
forceSync dimmers = D.runDimmers (getDimmers dimmers) $ \rs ->
        arrayMap $ \ix -> store (addrOf rs ! ix ~> D.synced) false



sync :: Dimmers -> Ivory (ProcEffects s ()) ()
sync Dimmers{..} = do
    shouldInit' <- deref shouldInit
    when (iNot shouldInit') $ do
        i <- deref current
        D.runDimmers getDimmers $ \ds -> do
            let d = addrOf ds ! toIx i
            synced' <- deref $ d ~> D.synced
            when (iNot synced') $ do
                msg <- D.message getDimmers (i .% n)
                transmit msg
                store (d ~> D.synced) true
        store current $ i + 1



onDo :: KnownNat n => Dimmers -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
onDo Dimmers{..} buff size = do
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



onDim :: KnownNat n => Dimmers -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
onDim Dimmers{..} buff size = do
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



onInit :: KnownNat n => Dimmers -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onInit Dimmers{..} buff size =
    when (size >=? 1 + n * 3) $ do
        offset <- local $ ival 1
        D.runDimmers getDimmers $ \ds -> do
            arrayMap $ \ix -> do
                offset' <- deref offset
                let d = addrOf ds ! ix
                group    <- unpack buff  offset'
                mode     <- unpack buff (offset' + 1)
                value    <- unpack buff (offset' + 2) :: Ivory eff Uint8
                D.initialize d group mode (safeCast value / 255) 0
                D.syncDimmerGroup ds d ix
                store offset $ offset' + 3
        store shouldInit false



onOn :: D.Dimmers -> Uint8 -> Ivory eff ()
onOn = D.on


onOff :: D.Dimmers -> Uint8 -> Ivory eff ()
onOff = D.off


onSet :: KnownNat n => D.Dimmers -> Uint8 -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
onSet dimmers index buff size =
    when (size >=? 4) $ do
        brightness <- unpack buff 3  :: Ivory eff Uint8
        D.setBrightness (safeCast brightness / 255) dimmers index


onFade :: KnownNat n => D.Dimmers -> Uint8 -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
onFade dimmers index buff size =
    when (size >=? 5) $ do
        value    <- unpack buff 3 :: Ivory eff Uint8
        velocity <- unpack buff 4 :: Ivory eff Uint8
        D.fade (safeCast value / 255) (safeCast velocity / 255) dimmers index


onMode :: KnownNat n => D.Dimmers -> Uint8 -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
onMode dimmers index buff size =
    when (size >=? 4) $ do
        mode <- unpack buff 3
        D.setMode mode dimmers index


onGroup :: KnownNat n => D.Dimmers -> Uint8 -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
onGroup dimmers index buff size =
    when (size >=? 4) $ do
        group <- unpack buff 3
        D.setGroup group dimmers index


onGetState ds@Dimmers{..} = do
    initialized <- iNot <$> deref shouldInit
    when initialized $ forceSync ds
