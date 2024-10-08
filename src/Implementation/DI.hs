{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Implementation.DI where

import           Control.Monad
import           Core.Actions
import           Core.Controller
import           Feature.ALED
import           Feature.DInputs (DInputs, forceSyncDInputs)
import           Feature.DS18B20
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib
import Control.Monad.State (MonadState)
import Core.Context
import Control.Monad.Reader (MonadReader)
import Core.Domain
import Interface.GPIO.Output
import Interface.GPIO.Port
import Control.Monad.RWS (asks)
import Interface.MCU (peripherals)
import Data.Value
import Core.Task
import Core.Transport


data DI n = DI
    { dinputs :: DInputs n
    , aled    :: ALED 10 100 2040
    }



di :: (MonadState Context m, MonadReader (Domain p c) m, Output o, Pull p u, Transport t) 
   => m t -> (Bool -> t -> m (DInputs n)) -> (p -> u -> m o) -> (t -> m (ALED 10 100 2040)) -> m (DI n)
di transport' dinputs' pin aled' = do
    transport <- transport'
    dinputs <- dinputs' True transport
    -- ds18b20 transport
    
    aled    <- aled' transport

    let name          = "blink"
    mcu              <- asks mcu
    let peripherals'  = peripherals mcu
    out              <- pin peripherals' $ pullNone peripherals'
    state            <- value (name <> "_state") false

    addTask $ yeld name $ do
        v <- deref state
        store state $ iNot v
        ifte_ v (set   out)
                (reset out)

    pure DI { dinputs, aled }


onGetState DI{..} buff size = do
    forceSyncDInputs dinputs
    forceSyncAled aled


instance KnownNat n => Controller (DI n) where
    handle d@DI{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionGetState               ==> onGetState               d    buff size
              , action ==? actionInitialize             ==> onInitialize             aled buff size
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
