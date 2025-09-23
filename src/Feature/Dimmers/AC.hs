{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.Dimmers.AC where

import Control.Monad (void)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import qualified Core.Domain as D
import Core.Handler
import Core.Task
import Core.Transport as T
import Data.Buffer
import Data.Fixed
import Data.Index
import Data.Record
import Data.Serialize
import Data.Value
import qualified Endpoint.Dimmers as Dim
import Feature.Dimmers
import GHC.TypeNats
import Interface.EXTI
import Interface.MCU
import qualified Interface.PWM as I
import Interface.Timer
import Ivory.Language
import Ivory.Stdlib
import Support.Cast
import Prelude hiding (head)

data CrossZero = CrossZero
    { isCrossZero :: Value IBool
    , isNoCrossZero :: Value IBool
    , countCrossZero :: Value Uint32
    , period0 :: Value Uint32
    , period1 :: Value Uint32
    }

dimmersAC ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , Handler HandleEXTI e
    , EXTI e
    , T.Transport t
    , I.PWM o
    , KnownNat n
    ) =>
    List n (p -> Uint32 -> Uint32 -> m o) ->
    (p -> m e) ->
    t ->
    m (Dimmers n)
dimmersAC pwms exti transport = do
    mcu <- asks D.mcu
    e <- exti $ peripherals mcu

    dimmers <- mkDimmers pwms 0xff_ff_ff_ff transport

    isCrossZero <- value "dimmer_is_zero" false
    isNoCrossZero <- value "dimmer_is_no_zero" true
    countCrossZero <- value "dimmer_count_zero" 0
    period0 <- value "dimmer_period_0" 0
    period1 <- value "dimmer_period_1" 0

    let crossZero =
            CrossZero
                { isCrossZero
                , isNoCrossZero
                , countCrossZero
                , period0
                , period1
                }

    addHandler $ HandleEXTI e $ detectCrossZero dimmers crossZero

    addTask $ delay 1_000 "dimmers_cross_zero_error" $ detectCrossZeroError dimmers crossZero
    addTask $ delay 10 "dimmers_manage_no_cross_zero" $ manageNoCrossZero dimmers crossZero
    addTask $ delay 1 "dimmers_calculate" $ calculate dimmers
    addTask $ yeld "dimmers_manage" $ manage dimmers crossZero

    pure dimmers

detectCrossZero :: Dimmers n -> CrossZero -> Ivory eff ()
detectCrossZero Dimmers{..} CrossZero{..} = do
    period1' <- deref period1
    store period1 =<< getCounter (head getPWMs)
    store period0 period1'
    mapM_ resetCounter getPWMs
    countCrossZero' <- deref countCrossZero
    store countCrossZero $ countCrossZero' + 1
    store isCrossZero true

{-
    TODO: Send a cross Zero error to the server
-}

detectCrossZeroError :: Dimmers n -> CrossZero -> Ivory eff ()
detectCrossZeroError Dimmers{..} CrossZero{..} = do
    countCrossZero' <- deref countCrossZero
    store isNoCrossZero $ countCrossZero' <? 75
    store countCrossZero 0

calculate :: (KnownNat n) => Dimmers n -> Ivory eff ()
calculate Dimmers{..} = zipWithM_ zip getPWMs nats
  where
    zip :: (I.PWM p) => p -> Int -> Ivory eff ()
    zip pwm i = do
        let ix = fromIntegral i
        let d = Dim.dimmers getDimmers ! ix
        calculateDimmer d

calculateDimmer :: Record Dim.DimmerStruct -> Ivory eff ()
calculateDimmer = Dim.calculateValue

manage :: (KnownNat n) => Dimmers n -> CrossZero -> Ivory eff ()
manage Dimmers{..} CrossZero{..} = do
    isCrossZero' <- deref isCrossZero
    isNoCrossZero' <- deref isNoCrossZero
    when (iNot isNoCrossZero' .&& isCrossZero') $ do
        zipWithM_ zip getPWMs ints
        store isCrossZero false
  where
    zip :: (I.PWM p) => p -> Int -> Ivory eff ()
    zip pwm i = do
        let ix = fromIntegral i
        let d = Dim.dimmers getDimmers ! ix
        manageDimmer pwm d =<< deref period0

manageDimmer :: (I.PWM p) => p -> Record Dim.DimmerStruct -> Uint32 -> Ivory eff ()
manageDimmer pwm dimmer period = do
    v <- deref $ dimmer ~> Dim.value
    cond_
        [ v ==? 0 ==> I.setMode pwm I.FORCE_LOW
        , v ==? 1 ==> I.setMode pwm I.FORCE_HIGH
        , true ==> do
            I.setMode pwm I.LOW
            I.setDuty pwm =<< castFloatToUint16 ((1 - v) * (safeCast period - 400) + 100)
        ]

manageNoCrossZero :: (KnownNat n) => Dimmers n -> CrossZero -> Ivory eff ()
manageNoCrossZero Dimmers{..} CrossZero{..} = do
    isNoCrossZero' <- deref isNoCrossZero
    when isNoCrossZero' $
        zipWithM_ zip getPWMs ints
  where
    zip :: (I.PWM p) => p -> Int -> Ivory eff ()
    zip pwm i = do
        let ix = fromIntegral i
        let d = Dim.dimmers getDimmers ! ix
        manageDimmerNoCrossZero pwm d

manageDimmerNoCrossZero :: (I.PWM p) => p -> Record Dim.DimmerStruct -> Ivory eff ()
manageDimmerNoCrossZero pwm dimmer = do
    v <- deref $ dimmer ~> Dim.value
    cond_
        [ v >? 0 ==> I.setMode pwm I.FORCE_HIGH
        , true ==> I.setMode pwm I.FORCE_LOW
        ]
