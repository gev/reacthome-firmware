module Feature.Dimmers.DC where

import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Core.Context
import Core.Domain qualified as D
import Core.Task
import Core.Transport as T
import Data.Fixed
import Data.Record
import Endpoint.Dimmers qualified as Dim
import Feature.Dimmers
import GHC.TypeNats
import Interface.PWM qualified as I
import Ivory.Language
import Ivory.Stdlib
import Support.Cast

dimmersDC ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , T.Transport t
    , I.PWM o
    , KnownNat n
    ) =>
    List n (p -> Uint32 -> Uint32 -> m o) ->
    t ->
    m (Dimmers n)
dimmersDC pwms transport = do
    dimmers <- mkDimmers pwms 1_000 transport

    addTask $ delay 1 "dimmers_manage" $ manage dimmers

    pure dimmers

manage :: (KnownNat n) => Dimmers n -> Ivory eff ()
manage Dimmers{..} = zipWithM_ zip getPWMs ints
  where
    zip :: (I.PWM p) => p -> Int -> Ivory eff ()
    zip pwm i = do
        let ix = fromIntegral i
        let d = Dim.dimmers getDimmers ! ix
        manageDimmer pwm d

manageDimmer ::
    (I.PWM p) =>
    p ->
    Record Dim.DimmerStruct ->
    Ivory eff ()
manageDimmer pwm dimmer = do
    Dim.calculateValue dimmer
    v <- deref $ dimmer ~> Dim.value
    cond_
        [ v ==? 0 ==> I.setMode pwm I.FORCE_LOW
        , v ==? 1 ==> I.setMode pwm I.FORCE_HIGH
        , true ==> do
            I.setMode pwm I.LOW
            I.setDuty pwm =<< castFloatToUint16 ((1 - v) * 960 + 40)
        ]
