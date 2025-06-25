{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
module Implementation.DILA where

import           Control.Monad
import           Control.Monad.Reader  (MonadReader, ask)
import           Control.Monad.RWS     (asks)
import           Control.Monad.State   (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Controller
import           Core.Domain           as D
import           Core.Task
import           Core.Transport
import           Data.Value
import           Feature.ALED
import           Feature.DInputs       (DInputs, forceSyncDInputs)
import           Feature.DS18B20
import           GHC.TypeNats
import           Interface.GPIO.Output
import           Interface.GPIO.Port
import           Interface.MCU         (peripherals)
import           Ivory.Language
import           Ivory.Stdlib

data DILA n = DILA
    { dinputs    :: DInputs n
    , aled       :: ALED 10 100 2040
    , shouldInit :: Value IBool
    }



dila :: MonadReader (D.Domain p c) m
     => m t
     -> (Bool -> t -> m (DInputs n))
     -> (t -> m DS18B20)
     -> (t -> m (ALED 10 100 2040))
     -> m (DILA n)
dila transport' dinputs' ds18b20 aled' = do
    transport  <- transport'
    shouldInit <- asks D.shouldInit
    dinputs    <- dinputs' True transport
    aled       <- aled' transport
    ds18b20 transport
    pure DILA { dinputs, aled, shouldInit }


onGetState DILA{..} buff size = do
    forceSyncDInputs dinputs
    forceSyncAled aled


instance KnownNat n => Controller (DILA n) where
    handle d@DILA{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionGetState               ==> onGetState               d    buff size
              , action ==? actionInitialize             ==> onInitialize             aled buff size shouldInit
              , action ==? actionALedOn                 ==> onALedOn                 aled buff size
              , action ==? actionALedOff                ==> onALedOff                aled buff size
              , action ==? actionALedColorAnimationPlay ==> onALedColorAnimationPlay aled buff size
              , action ==? actionALedColorAnimationStop ==> onALedColorAnimationStop aled buff size
              , action ==? actionALedMaskAnimationPlay  ==> onALedMaskAnimationPlay  aled buff size
              , action ==? actionALedMaskAnimationStop  ==> onALedMaskAnimationStop  aled buff size
              , action ==? actionALedClip               ==> onALedClip               aled buff size
              , action ==? actionALedBrightness         ==> onALedBrightness         aled buff size
              , action ==? actionALedConfigGroup        ==> onALedConfigGroup        aled buff size
              ]
