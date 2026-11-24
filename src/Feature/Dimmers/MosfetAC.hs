module Feature.Dimmers.MosfetAC where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Context
import Core.Domain qualified as D
import Core.Handler
import Core.Meta
import Core.Task
import Core.Transport as T
import Data.Fixed
import Data.Record
import Data.Value
import Endpoint.Dimmers qualified as Dim
import Feature.Dimmers
import GHC.TypeNats
import Interface.EXTI
import Interface.MCU
import Interface.MCU qualified as I
import Interface.PWM qualified as I
import Interface.Timer
import Ivory.Language
import Ivory.Stdlib
import Support.Cast
import Prelude hiding (head)

risingEdgeMode = 1
fallingEdgeMode = 2
pwmMode = 3

data CrossZero = CrossZero
    { isCrossZero :: Value IBool
    , isNoCrossZero :: Value IBool
    , countCrossZero :: Value Uint32
    , period0 :: Value Uint32
    , period1 :: Value Uint32
    , pdmPhase :: Value IBool
    }

dimmersMosfetAC ::
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
dimmersMosfetAC pwms exti transport = do
    meta <- asks D.meta
    platform <- I.platform meta.mcu

    e <- exti $ peripherals platform

    dimmers <- mkDimmers pwms 0xff_ff_ff_ff transport

    isCrossZero <- value "dimmer_is_zero" false
    isNoCrossZero <- value "dimmer_is_no_zero" true
    countCrossZero <- value "dimmer_count_zero" 0
    period0 <- value "dimmer_period_0" 0
    period1 <- value "dimmer_period_1" 0
    pdmPhase <- value "dimmer_pdm_phase" false

    let crossZero =
            CrossZero
                { isCrossZero
                , isNoCrossZero
                , countCrossZero
                , period0
                , period1
                , pdmPhase
                }

    addHandler $ HandleEXTI e $ detectCrossZero dimmers crossZero

    addTask $ delay 1_000 "dimmers_cross_zero_error" $ detectCrossZeroError crossZero
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

detectCrossZeroError :: CrossZero -> Ivory eff ()
detectCrossZeroError CrossZero{..} = do
    countCrossZero' <- deref countCrossZero
    store isNoCrossZero $ countCrossZero' <? 75
    store countCrossZero 0

calculate :: (KnownNat n) => Dimmers n -> Ivory eff ()
calculate Dimmers{..} =
    arrayMap \ix -> do
        let d = Dim.dimmers getDimmers ! ix
        calculateDimmer d

calculateDimmer :: Record Dim.DimmerStruct -> Ivory eff ()
calculateDimmer = Dim.calculateValue

manage :: (KnownNat n) => Dimmers n -> CrossZero -> Ivory eff ()
manage Dimmers{..} CrossZero{..} = do
    isCrossZero' <- deref isCrossZero
    isNoCrossZero' <- deref isNoCrossZero
    when (iNot isNoCrossZero' .&& isCrossZero') do
        phase <- deref pdmPhase
        zipWithM_ (zip phase) getPWMs ints
        -- PDM works on full periods: step on every second cross zero
        store pdmPhase $ iNot phase
        store isCrossZero false
  where
    zip :: (I.PWM p) => IBool -> p -> Int -> Ivory eff ()
    zip phase pwm i = do
        let ix = fromIntegral i
        let d = Dim.dimmers getDimmers ! ix
        manageDimmer pwm d (iNot phase) =<< deref period0

manageDimmer :: (I.PWM p) => p -> Record Dim.DimmerStruct -> IBool -> Uint32 -> Ivory eff ()
manageDimmer pwm dimmer step period = do
    v <- deref $ dimmer ~> Dim.value
    mode <- deref $ dimmer ~> Dim.mode
    cond_
        [ v ==? 0 ==> do
            store (dimmer ~> Dim.pdm) 0
            I.setMode pwm I.FORCE_LOW
        , v ==? 1 ==> do
            store (dimmer ~> Dim.pdm) 0
            I.setMode pwm I.FORCE_HIGH
        , true ==> do
            cond_
                [ mode ==? pwmMode ==> when step do
                    -- PDM: sigma-delta accumulator, one step per period (every second half-period)
                    level <- castFloatToUint16 (v * 255 + 0.5)
                    acc <- deref $ dimmer ~> Dim.pdm
                    let acc' = acc + level
                    ifte_
                        (acc' >=? 255)
                        ( do
                            store (dimmer ~> Dim.pdm) (acc' - 255)
                            I.setMode pwm I.FORCE_HIGH
                        )
                        ( do
                            store (dimmer ~> Dim.pdm) acc'
                            I.setMode pwm I.FORCE_LOW
                        )
                , mode ==? risingEdgeMode ==> do
                    I.setMode pwm I.LOW
                    I.setDuty pwm =<< castFloatToUint16 ((1 - v) * (safeCast period - 400) + 100)
                , mode ==? fallingEdgeMode ==> do
                    I.setMode pwm I.HIGH
                    I.setDuty pwm =<< castFloatToUint16 (v * (safeCast period - 400) + 100)
                ]
        ]

manageNoCrossZero :: (KnownNat n) => Dimmers n -> CrossZero -> Ivory eff ()
manageNoCrossZero Dimmers{..} CrossZero{..} = do
    isNoCrossZero' <- deref isNoCrossZero
    when isNoCrossZero' do
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
    mode <- deref $ dimmer ~> Dim.mode
    cond_
        [ v >? 0 .&& mode /=? pwmMode ==> I.setMode pwm I.FORCE_HIGH
        , true ==> I.setMode pwm I.FORCE_LOW
        ]
