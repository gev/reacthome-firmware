{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Feature.Dimmers.DC where

import           Control.Monad        (zipWithM_)
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State  (MonadState)
import           Core.Actions
import           Core.Context
import qualified Core.Domain          as D
import           Core.Task
import           Core.Transport       as T
import           Data.Buffer
import           Data.Index
import           Data.Record
import           Data.Serialize
import           Data.Value
import qualified Endpoint.Dimmers     as Dim
import           Feature.Dimmers
import           GHC.TypeNats
import           Interface.MCU
import qualified Interface.PWM        as I
import           Ivory.Language
import           Ivory.Stdlib
import           Support.Cast



dimmersDC :: ( MonadState Context m
             , MonadReader (D.Domain p c) m
             , T.Transport t, I.PWM o
             ) => [p -> Uint32 -> Uint32 -> m o] -> t -> m Dimmers
dimmersDC pwms transport = do
    dimmers <- mkDimmers pwms 1_000 transport

    addTask $ delay 1 "dimmers_manage" $ manage dimmers

    pure dimmers



manage :: Dimmers -> Ivory eff ()
manage Dimmers{..} = zipWithM_ zip getPWMs [0..]
    where
        zip :: I.PWM p => p -> Int -> Ivory eff ()
        zip pwm i = Dim.runDimmers getDimmers $ \ds -> do
            let ix = fromIntegral i
            let d = addrOf ds ! ix
            manageDimmer pwm d



manageDimmer :: I.PWM p => p -> Record Dim.DimmerStruct -> Ivory eff ()
manageDimmer pwm dimmer = do
    Dim.calculateValue dimmer
    v <- deref $ dimmer ~> Dim.value
    cond_ [ v ==? 0 ==> I.setMode pwm I.FORCE_LOW
          , v ==? 1 ==> I.setMode pwm I.FORCE_HIGH
          , true ==> do I.setMode pwm I.LOW
                        I.setDuty pwm =<< castFloatToUint16 ((1 - v) * 960 + 40)
          ]
